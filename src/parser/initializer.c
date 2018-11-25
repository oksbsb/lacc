#if !AMALGAMATION
# define INTERNAL
# define EXTERNAL extern
#endif
#include "eval.h"
#include "expression.h"
#include "initializer.h"
#include "parse.h"
#include "typetree.h"
#include <lacc/context.h>
#include <lacc/token.h>

#include <assert.h>

static struct block *initialize_member(
    struct preprocessor *input,
    struct definition *def,
    struct block *block,
    struct block *values,
    struct var target);

/*
 * Evaluate sequence of IR_ASSIGN statements in a separate block. This
 * is appended at the end after all expressions inside initializers are
 * evaluated.
 *
 * Padding initialization is handled only after the whole initializer is
 * read, as postprocessing of the statements in this block.
 */
static struct block *get_initializer_block(int i)
{
    static struct block *block[3];

    return !block[i] ? (block[i] = cfg_block_init(NULL)) : block[i];
}

static struct block *read_initializer_element(
    struct preprocessor *input,
    struct definition *def,
    struct block *block,
    struct var target)
{
    size_t ops;
    struct var value;
    const struct block *top;

    ops = array_len(&block->code);
    top = block;
    block = assignment_expression(input, def, block);
    value = block->expr.l;

    if (target.symbol->linkage != LINK_NONE) {
        if (block != top
            || array_len(&block->code) - ops > 0
            || !is_identity(block->expr)
            || (!is_constant(value) && value.kind != ADDRESS
                && !(value.kind == DIRECT
                    && (is_function(value.type) || is_array(value.type)))))
        {
            error(input, "Initializer must be computable at load time.");
            exit(1);
        }
    }

    return block;
}

enum current_object_state {
    CURRENT,        /* Current object. */
    DESIGNATOR,     /* In designator. */
    MEMBER
};

static int next_element(
    struct preprocessor *input,
    enum current_object_state state)
{
    struct token t = peek(input);
    if (t.token == ',') {
        switch (peekn(input, 2).token) {
        case '}':
            break;
        case '.':
            if (state != CURRENT) {
                break;
            }
        default:
            next(input);
            return 1;
        }
    }

    return 0;
}

static struct var access_member(
    struct var target,
    const struct member *member,
    size_t offset)
{
    target.type = member->type;
    target.field_offset = member->field_offset;
    target.field_width = member->field_width;
    target.offset = offset + member->offset;
    return target;
}

static const struct member *get_named_member(
    struct preprocessor *input,
    Type type,
    String name,
    int *i)
{
    const struct member *member;

    member = find_type_member(type, name, i);
    if (member == NULL) {
        error(input, "%t has no member named %s.", type, str_raw(name));
        exit(1);
    }

    return member;
}

/*
 * Initialize the first union member, or the last member specified by a
 * designator.
 *
 * If the initialized element is not also the largest member, or if
 * there is padding, the remaining memory is undefined.
 *
 * With designators, there can be arbitrary many member initializers,
 * but only the last one should count. Evaluate each member in its own
 * block to cleanly reset.
 *
 *     union {
 *         struct { int x, y; } p;
 *         int q;
 *     } foo = {{1, 2}, .q = 3};
 *
 * In the above definition, we want the value of foo.p.y to be 0, even
 * though the assignment to .q does not overwrite it.
 */
static struct block *initialize_union(
    struct preprocessor *input,
    struct definition *def,
    struct block *block,
    struct block *values,
    struct var target,
    enum current_object_state state)
{
    int done;
    size_t filled;
    struct block *init;
    const struct member *member;
    Type type;
    String name;

    done = 0;
    filled = target.offset;
    type = target.type;
    init = get_initializer_block(2);
    assert(is_union(type));
    assert(nmembers(type) > 0);

    do {
        if (peek(input).token == '.') {
            next(input);
            name = consume(input, IDENTIFIER).d.string;
            member = get_named_member(input, type, name, NULL);
            target = access_member(target, member, filled);
            if (peek(input).token == '=') {
                next(input);
            }
        } else if (!done) {
            member = get_member(type, 0);
            target = access_member(target, member, filled);
        } else break;
        array_empty(&init->code);
        block = initialize_member(input, def, block, init, target);
        done = 1;
    } while (next_element(input, state));

    array_concat(&values->code, &init->code);
    return block;
}

/*
 * Initialize members of a struct.
 *
 * Members of structs can have overlapping offsets from anonymous union
 * fields. Act as if only the first element is initialized by skipping
 * all consecutive elements with the same offset.
 */
static struct block *initialize_struct(
    struct preprocessor *input,
    struct definition *def,
    struct block *block,
    struct block *values,
    struct var target,
    enum current_object_state state)
{
    int i, m;
    size_t filled;
    Type type;
    String name;
    const struct member *member, *prev;

    prev = NULL;
    target.lvalue = 1;
    filled = target.offset;
    type = target.type;
    assert(is_struct(type));
    assert(nmembers(type) > 0);

    m = nmembers(type);
    i = 0;
    do {
        if (peek(input).token == '.') {
            next(input);
            name = consume(input, IDENTIFIER).d.string;
            member = get_named_member(input, type, name, &i);
            target = access_member(target, member, filled);
            if (peek(input).token == '=') {
                next(input);
            }
            block = initialize_member(input, def, block, values, target);
            prev = member;
            i += 1;
        } else {
            while (1) {
                member = get_member(type, i);
                i += 1;
                if (!prev
                    || prev->offset != member->offset
                    || prev->field_offset != member->field_offset)
                    break;
            }
            prev = member;
            target = access_member(target, member, filled);
            block = initialize_member(input, def, block, values, target);
            if (i >= m)
                break;
        }
    } while (next_element(input, state));

    return block;
}

static struct block *initialize_struct_or_union(
    struct preprocessor *input,
    struct definition *def,
    struct block *block,
    struct block *values,
    struct var target,
    enum current_object_state state)
{
    assert(is_struct_or_union(target.type));
    assert(nmembers(target.type) > 0);

    if (is_union(target.type)) {
        block = initialize_union(input, def, block, values, target, state);
    } else {
        block = initialize_struct(input, def, block, values, target, state);
    }

    return block;
}

static int next_array_element(
    struct preprocessor *input,
    enum current_object_state state)
{
    struct token t = peek(input);
    if (t.token == ',') {
        t = peekn(input, 2);
        switch (t.token) {
        case '}':
        case '.':
            break;
        case '[':
            if (state != CURRENT) {
                break;
            }
        default:
            next(input);
            return 1;
        }
    }

    return 0;
}

static int try_parse_index(struct preprocessor *input, size_t *index)
{
    struct var num;

    if (peek(input).token == '[') {
        next(input);
        num = constant_expression(input);
        if (!is_integer(num.type)) {
            error(input, "Array designator must have integer value.");
            exit(1);
        }
        consume(input, ']');
        *index = num.imm.i;
        return 1;
    }

    return 0;
}

/*
 * Initialize array types with brace-enclosed values, or string literal.
 *
 *     a[] = {1, 2, 3};
 *     b[] = "Hello world"
 *     c[2][3] = {1, 2, 3, {4, 5, 6}}
 *
 * Handle special case of incomplete array type, and assignment to
 * arrays which are longer than the string itself. In that case, the
 * rest of the array is initialized to zero.
 *
 *      char foo[5] = "Hi"
 *
 * This will generates the following IR assignments, after padding is
 * added at the end:
 *
 *      foo = "Hi"
 *      foo[3] = 0
 *      foo[4] = 0
 */
static struct block *initialize_array(
    struct preprocessor *input,
    struct definition *def,
    struct block *block,
    struct block *values,
    struct var target,
    enum current_object_state state)
{
    Type type, elem;
    size_t initial, width, i;

    assert(is_array(target.type));
    assert(target.kind == DIRECT);

    i = 0;
    type = target.type;
    elem = type_next(type);
    width = size_of(elem);
    initial = target.offset;

    /*
     * Need to read expression to determine if element is a string
     * constant, or an integer like "Hello"[2].
     */
    if (is_char(elem) && peek(input).token != '[') {
        block = read_initializer_element(input, def, block, target);
        if (is_identity(block->expr) && is_string(block->expr.l)) {
            target = eval_assign(input, def, values, target, block->expr);
        } else {
            target.type = elem;
            eval_assign(input, def, values, target, block->expr);
            goto next;
        }
    } else {
        target.type = elem;
        do {
            if (try_parse_index(input, &i) && peek(input).token == '=') {
                next(input);
            }
            target.offset = initial + (i * width);
            block = initialize_member(input, def, block, values, target);
next:       i += 1;
        } while (next_array_element(input, state));
    }

    if (!size_of(type)) {
        assert(is_array(target.symbol->type));
        assert(!size_of(target.symbol->type));
        set_array_length(target.symbol->type, i);
    }

    return block;
}

static struct block *initialize_member(
    struct preprocessor *input,
    struct definition *def,
    struct block *block,
    struct block *values,
    struct var target)
{
    assert(target.kind == DIRECT);
    if (is_struct_or_union(target.type)) {
        if (peek(input).token == '{') {
            next(input);
            block = initialize_struct_or_union(input, def, block, values, target, CURRENT);
            if (peek(input).token == ',')
                next(input);
            consume(input, '}');
        } else {
            block = initialize_struct_or_union(input, def, block, values, target, DESIGNATOR);
        }
    } else if (is_array(target.type)) {
        if (!size_of(target.type)) {
            error(input, "Invalid initialization of flexible array member.");
            exit(1);
        }
        if (peek(input).token == '{') {
            next(input);
            block = initialize_array(input, def, block, values, target, CURRENT);
            if (peek(input).token == ',')
                next(input);
            consume(input, '}');
        } else {
            block = initialize_array(input, def, block, values, target, DESIGNATOR);
        }
    } else {
        if (peek(input).token == '{') {
            next(input);
            block = read_initializer_element(input, def, block, target);
            consume(input, '}');
        } else {
            block = read_initializer_element(input, def, block, target);
        }
        eval_assign(input, def, values, target, block->expr);
    }

    return block;
}

static struct block *initialize_object(
    struct preprocessor *input,
    struct definition *def,
    struct block *block,
    struct block *values,
    struct var target)
{
    assert(target.kind == DIRECT);

    if (peek(input).token == '{') {
        next(input);
        if (is_struct_or_union(target.type)) {
            block = initialize_struct_or_union(input, def, block, values, target, CURRENT);
        } else if (is_array(target.type)) {
            block = initialize_array(input, def, block, values, target, CURRENT);
        } else {
            block = initialize_object(input, def, block, values, target);
        }
        if (peek(input).token == ',') {
            next(input);
        }
        consume(input, '}');
    } else if (is_array(target.type)) {
        block = initialize_array(input, def, block, values, target, MEMBER);
    } else {
        block = read_initializer_element(input, def, block, target);
        eval_assign(input, def, values, target, block->expr);
    }

    return block;
}

static const struct var var__immediate_zero = {IMMEDIATE, {T_INT}};

/*
 * Set var = 0, using simple assignment on members for composite types.
 *
 * This rule does not consume any input, but generates a series of
 * assignments on the given variable. Point is to be able to zero
 * initialize using normal simple assignment rules, although IR can
 * become verbose for large structures.
 */
static void zero_initialize(
    struct definition *def,
    struct block *values,
    struct var target)
{
    int i;
    size_t size;
    struct var var;

    assert(target.kind == DIRECT);
    size = size_of(target.type);
    switch (type_of(target.type)) {
    case T_STRUCT:
    case T_UNION:
        assert(size);
        target.type = (size % 8)
            ? type_create_array(basic_type__char, size)
            : type_create_array(basic_type__long, size / 8);
    case T_ARRAY:
        var = target;
        target.type = type_next(target.type);
        for (i = 0; i < size / size_of(target.type); ++i) {
            target.offset = var.offset + i * size_of(target.type);
            zero_initialize(def, values, target);
        }
        break;
    case T_BOOL:
    case T_CHAR:
    case T_SHORT:
    case T_INT:
    case T_LONG:
    case T_FLOAT:
    case T_DOUBLE:
    case T_LDOUBLE:
    case T_POINTER:
        var = var__immediate_zero;
        var.type = target.type;
        eval_assign(NULL, def, values, target, as_expr(var));
        break;
    default:
        error(NULL, "Cannot zero-initialize object of type '%t'.", target.type);
        exit(1);
    }
}

static void zero_initialize_bytes(
    struct definition *def,
    struct block *values,
    struct var target,
    size_t bytes)
{
    size_t size;

    target.field_offset = 0;
    target.field_width = 0;
    while (bytes) {
        size = bytes % 8;
        if (!size) {
            size = 8;
        }

        assert(size <= bytes);
        switch (size) {
        default:
            size = 1;
        case 1:
            target.type = basic_type__char;
            break;
        case 2:
            target.type = basic_type__short;
            break;
        case 4:
            target.type = basic_type__int;
            break;
        case 8:
            target.type = basic_type__long;
            break;
        }

        zero_initialize(def, values, target);
        target.offset += size_of(target.type);
        bytes -= size;
    }
}

/*
 * Zero initialize padding bytes between target reference and next field
 * assignment.
 *
 * Target has offset and field offset pointing to first location not yet
 * initialized.
 */
static void initialize_padding(
    struct definition *def,
    struct block *block,
    struct var target,
    struct var field)
{
    size_t bits;
    size_t padding;

    if (target.offset < field.offset) {
        if (target.field_offset) {
            bits = size_of(target.type) * 8;
            target.field_width = bits - target.field_offset;
            zero_initialize(def, block, target);
            target.offset += size_of(target.type);
            target.field_offset = 0;
            target.field_width = 0;
        }

        padding = field.offset - target.offset;
        zero_initialize_bytes(def, block, target, padding);
    } else if (target.field_offset < field.field_offset) {
        target.field_width = field.field_offset - target.field_offset;
        zero_initialize(def, block, target);
    }
}

/*
 * Initialize padding at the end of object.
 *
 * Consider both last bits of a bitfield, and any remaining space after
 * the bitfield itself.
 */
static void initialize_trailing_padding(
    struct definition *def,
    struct block *block,
    struct var target,
    size_t size,
    size_t bitfield_size)
{
    assert(size >= target.offset);

    if (target.field_offset) {
        switch (bitfield_size) {
        case 1:
            target.type = basic_type__char;
            target.field_width = 8 - target.field_offset;
            break;
        case 2:
            target.type = basic_type__short;
            target.field_width = 16 - target.field_offset;
            break;
        case 4:
            target.type = basic_type__int;
            target.field_width = 32 - target.field_offset;
            break;
        default:
            assert(bitfield_size == 8);
            target.type = basic_type__long;
            target.field_width = 64 - target.field_offset;
            break;
        }

        assert(target.field_width > 0);
        zero_initialize(def, block, target);
        target.offset += size_of(target.type);
    }

    assert(size >= target.offset);
    if (size > target.offset) {
        zero_initialize_bytes(def, block, target, size - target.offset);
    }
}

#ifndef NDEBUG
static void validate_initializer_block(struct block *block)
{
    int i;
    struct statement st;
    struct var target = {0}, field;

    for (i = 0; i < array_len(&block->code); ++i) {
        st = array_get(&block->code, i);
        assert(st.st == IR_ASSIGN);
        assert(target.offset <= st.t.offset);
        field = st.t;
        if (target.offset < field.offset) {
            assert(field.offset - target.offset == size_of(target.type));
        } else {
            assert(target.offset == field.offset);
            assert(target.field_offset + target.field_width
                == field.field_offset);
        }

        target = field;
    }
}
#endif

/*
 * Reorder initializer assignments to increasing offsets, and remove
 * duplicate assignments to the same element.
 */
static void sort_and_trim(struct block *values)
{
    int i, j;
    struct statement *code, tmp;

    code = &array_get(&values->code, 0);
    for (i = 1; i < array_len(&values->code); ++i) {
        j = i - 1;
        while (j >= 0 && code[j].t.offset > code[j + 1].t.offset) {
            tmp = code[j];
            code[j] = code[j + 1];
            code[j + 1] = tmp;
            if (j == 0) {
                break;
            } else {
                j--;
            }
        }

        if (code[j].t.offset == code[j + 1].t.offset
            && code[j].t.field_offset == code[j + 1].t.field_offset)
        {
            assert(code[j].t.field_width == code[j + 1].t.field_width);
            array_erase(&values->code, j);
            i -= 1;
        }
    }
}

/*
 * Fill in any missing padding initialization in assignment statement
 * list.
 *
 * The input block contains a list of assignments to the same variable,
 * possibly sparsely covering the full size of the type.
 */
static struct block *postprocess_object_initialization(
    struct definition *def,
    struct block *values,
    struct var target)
{
    int i, bitfield_size;
    size_t total_size;
    struct statement st;
    struct var field;
    struct block *block;

    assert(target.offset == 0);
    sort_and_trim(values);
    block = get_initializer_block(1);
    total_size = size_of(target.type);
    bitfield_size = 0;

    for (i = 0; i < array_len(&values->code); ++i) {
        st = array_get(&values->code, i);
        field = st.t;
        if (i == 0) {
            target.type = field.type;
        }

        assert(st.st == IR_ASSIGN);
        assert(field.offset >= 0);
        assert(target.offset <= field.offset);

        initialize_padding(def, block, target, field);
        array_push_back(&block->code, st);
        target.type = field.type;
        target.offset = field.offset;
        if (field.field_width) {
            if (size_of(field.type) > bitfield_size) {
                bitfield_size = size_of(field.type);
            }
            target.field_offset = field.field_offset + field.field_width;
            target.field_width = 0;
            if (target.field_offset == bitfield_size * 8) {
                target.field_offset = 0;
                target.offset += bitfield_size;
            }
        } else {
            target.field_offset = 0;
            target.field_width = 0;
            target.offset += size_of(field.type);
            bitfield_size = 0;
        }
    }

    initialize_trailing_padding(def, block, target, total_size, bitfield_size);
    array_empty(&values->code);
#ifndef NDEBUG
    validate_initializer_block(block);
#endif
    return block;
}

INTERNAL struct block *initializer(
    struct preprocessor *input,
    struct definition *def,
    struct block *block,
    const struct symbol *sym)
{
    struct block *values = get_initializer_block(0);
    struct var target = var_direct(sym);

    if (peek(input).token == '{' || is_array(sym->type)) {
        block = initialize_object(input, def, block, values, target);
        values = postprocess_object_initialization(def, values, target);
        array_concat(&block->code, &values->code);
        array_empty(&values->code);
    } else {
        block = read_initializer_element(input, def, block, target);
        eval_assign(input, def, block, target, block->expr);
    }

    return block;
}
