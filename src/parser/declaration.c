#if !AMALGAMATION
# define INTERNAL
# define EXTERNAL extern
#endif
#include "declaration.h"
#include "eval.h"
#include "expression.h"
#include "initializer.h"
#include "parse.h"
#include "statement.h"
#include "symtab.h"
#include "typetree.h"
#include <lacc/context.h>
#include <lacc/token.h>

#include <assert.h>
#include <limits.h>

static const Type *get_typedef(String str)
{
    struct symbol *tag;

    tag = sym_lookup(&ns_ident, str);
    if (tag && tag->symtype == SYM_TYPEDEF) {
        return &tag->type;
    }

    return NULL;
}

static struct block *parameter_declarator(
    struct preprocessor *input,
    struct definition *def,
    struct block *block,
    Type base,
    Type *type,
    String *name,
    size_t *length);

/*
 * Parse a function parameter list, adding symbols to scope.
 *
 * FOLLOW(parameter-list) = { ')' }, peek to return empty list; even
 * though K&R require at least specifier: (void)
 * Set parameter-type-list = parameter-list, including the , ...
 *
 * As a special case, ignore evaluation when in block scope. This is to
 * avoid VLA code that would be generated in cases like this:
 *
 *     int main(void) {
 *         int foo(int n, int arr[][n + 1]);
 *         return 0;
 *     }
 *
 * The evaluation of n + 1 is done in a throwaway block, and not
 * included in the CFG of main.
 */
static struct block *parameter_list(
    struct preprocessor *input,
    struct definition *def,
    struct block *parent,
    Type base,
    Type *func)
{
    String name;
    size_t length;
    int is_register;
    struct block *block;
    struct member *param;

    *func = type_create_function(base);
    block = current_scope_depth(&ns_ident) == 1
        ? parent
        : cfg_block_init(def);

    while (peek(input).token != ')') {
        name.len = 0;
        length = 0;
        base = declaration_specifiers(input, NULL, NULL, &is_register);
        block = parameter_declarator(input,
            def, block, base, &base, &name, &length);
        if (is_void(base)) {
            if (nmembers(*func)) {
                error(input, "Incomplete type in parameter list.");
                exit(1);
            }
            type_seal(input, *func);
            break;
        } else if (is_array(base)) {
            base = type_create_pointer(type_next(base));
        } else if (is_function(base)) {
            base = type_create_pointer(base);
        }
        param = type_add_member(input, *func, name, base);
        param->offset = length;
        if (name.len) {
            param->sym = sym_add(input, &ns_ident, name, base,
                SYM_DEFINITION, LINK_NONE);
        }
        if (peek(input).token != ',') {
            break;
        }
        consume(input, ',');
        if (peek(input).token == DOTS) {
            consume(input, DOTS);
            assert(!is_vararg(*func));
            type_add_member(input, *func, str_init("..."), basic_type__void);
            assert(is_vararg(*func));
            break;
        }
    }

    return current_scope_depth(&ns_ident) == 1 ? block : parent;
}

/*
 * Old-style function definitions with separate identifiers list and
 * type declarations.
 *
 * Return a function type where all members have placeholder type.
 */
static Type identifier_list(struct preprocessor *input, Type base)
{
    struct token t;
    Type type;

    type = type_create_function(base);
    if (peek(input).token != ')') {
        while (1) {
            t = consume(input, IDENTIFIER);
            if (get_typedef(t.d.string)) {
                error(input, "Unexpected type '%t' in identifier list.");
                exit(1);
            }
            type_add_member(input, type, t.d.string, get_type_placeholder());
            if (peek(input).token == ',') {
                next(input);
            } else break;
        }
    }

    return type;
}

struct array_param {
    char is_const;
    char is_volatile;
    char is_restrict;
    char is_static;
};

static void array_param_qualifiers(
    struct preprocessor *input,
    struct array_param *cvrs)
{
    assert(cvrs);
    if (peek(input).token == STATIC) {
        next(input);
        cvrs->is_static = 1;
    }

    while (1) {
        switch (peek(input).token) {
        case CONST:
            cvrs->is_const = 1;
            next(input);
            continue;
        case VOLATILE:
            cvrs->is_volatile = 1;
            next(input);
            continue;
        case RESTRICT:
            cvrs->is_restrict = 1;
            next(input);
            continue;
        default:
            break;
        }
        break;
    }

    if (peek(input).token == STATIC && !cvrs->is_static) {
        next(input);
        cvrs->is_static = 1;
    }
}

/*
 * Parse array declarations of the form [s0][s1]..[sn], resulting in
 * type [s0] [s1] .. [sn] (base).
 *
 * Only the first dimension s0 can be unspecified, yielding an
 * incomplete type. Incomplete types are represented by having size of
 * zero.
 *
 * VLA require evaluating an expression, and storing it in a separate
 * stack allocated variable.
 */
static struct block *array_declarator(
    struct preprocessor *input,
    struct definition *def,
    struct block *block,
    Type base,
    Type *type,
    size_t *static_length)
{
    size_t length = 0;
    struct var val;
    struct array_param cvrs = {0};
    int is_incomplete = 0;
    const struct symbol *sym = NULL;

    consume(input, '[');
    if (peek(input).token == ']') {
        is_incomplete = 1;
    } else {
        if (!def) {
            val = constant_expression(input);
            block = cfg_block_init(NULL);
        } else {
            assert(block);
            if (static_length) {
                array_param_qualifiers(input, &cvrs);
                if (cvrs.is_static && peek(input).token == ']') {
                    error(input, "Missing array length.");
                    exit(1);
                }
            }
            if (peek(input).token != ']') {
                block = assignment_expression(input, def, block);
                val = eval(def, block, block->expr);
            } else {
                is_incomplete = 1;
            }
        }
    }

    if (!is_incomplete) {
        if (!is_integer(val.type)) {
            error(input, "Array dimension must be of integer type.");
            exit(1);
        }
        if (val.kind == IMMEDIATE && is_signed(val.type) && val.imm.i < 0) {
            error(input, "Array dimension must be a positive number.");
            exit(1);
        }

        if (!type_equal(val.type, basic_type__unsigned_long)) {
            val = eval(def, block,
                eval_expr(input, def, block, IR_OP_CAST, val,
                    basic_type__unsigned_long));
        } else if (val.kind == DIRECT && !is_temporary(val.symbol)) {
            val = eval_copy(input, def, block, val);
        }

        assert(is_unsigned(val.type));
        assert(type_equal(val.type, basic_type__unsigned_long));

        block->expr = as_expr(val);
        if (val.kind == IMMEDIATE) {
            length = val.imm.u;
        } else {
            assert(val.kind == DIRECT);
            assert(val.symbol);
            sym = val.symbol;
        }
    }

    consume(input, ']');
    if (peek(input).token == '[') {
        block = array_declarator(input, def, block, base, &base, NULL);
    }

    if (!is_complete(base)) {
        error(input, "Array has incomplete element type.");
        exit(1);
    }

    if (static_length) {
        *static_length = length;
        *type = type_create_pointer(base);
        if (cvrs.is_const) *type = type_set_const(*type);
        if (cvrs.is_volatile) *type =  type_set_volatile(*type);
        if (cvrs.is_restrict) *type = type_set_restrict(*type);
    } else if (is_incomplete) {
        *type = type_create_incomplete(base);
    } else if (sym) {
        *type = type_create_vla(base, sym);
    } else {
        if (length * size_of(base) > LONG_MAX) {
            error(input, "Array is too large (%lu elements).", length);
            exit(1);
        }

        *type = type_create_array(base, length);
    }

    return block;
}

/*
 * Parse function and array declarators.
 *
 * Example:
 *
 *    void (*foo)(int)
 *
 * Traverse (*foo) first, and prepended on the outer `(int) -> void`,
 * making it `* (int) -> void`. Void is used as a sentinel, the inner
 * declarator can only produce pointer, function or array.
 */
static struct block *direct_declarator(
    struct preprocessor *input,
    struct definition *def,
    struct block *block,
    Type base,
    Type *type,
    String *name,
    size_t *length)
{
    struct token t;
    Type head = basic_type__void;

    switch (peek(input).token) {
    case IDENTIFIER:
        t = next(input);
        if (!name) {
            error(input, "Unexpected identifier in abstract declarator.");
            exit(1);
        }
        *name = t.d.string;
        break;
    case '(':
        next(input);
        block = declarator(input, def, block, head, &head, name);
        consume(input, ')');
        if (!is_void(head)) {
            length = NULL;
        }
        break;
    default:
        break;
    }

    switch (peek(input).token) {
    case '[':
        block = array_declarator(input, def, block, base, type, length);
        break;
    case '(':
        next(input);
        t = peek(input);
        push_scope(&ns_tag);
        push_scope(&ns_ident);
        if (t.token == IDENTIFIER && !get_typedef(t.d.string)) {
            *type = identifier_list(input, base);
        } else {
            block = parameter_list(input, def, block, base, type);
        }
        pop_scope(&ns_ident);
        pop_scope(&ns_tag);
        consume(input, ')');
        break;
    default:
        *type = base;
        break;
    }

    if (!is_void(head)) {
        *type = type_patch_declarator(head, *type);
    }

    return block;
}

static Type pointer(struct preprocessor *input, Type type)
{
    type = type_create_pointer(type);
    while (1) {
        next(input);
        switch (peek(input).token) {
        case CONST:
            type = type_set_const(type);
            break;
        case VOLATILE:
            type = type_set_volatile(type);
            break;
        case RESTRICT:
            type = type_set_restrict(type);
            break;
        default:
            return type;
        }
    }
}

static struct block *parameter_declarator(
    struct preprocessor *input,
    struct definition *def,
    struct block *block,
    Type base,
    Type *type,
    String *name,
    size_t *length)
{
    assert(type);
    while (peek(input).token == '*') {
        base = pointer(input, base);
    }

    return direct_declarator(input, def, block, base, type, name, length);
}

INTERNAL struct block *declarator(
    struct preprocessor *input,
    struct definition *def,
    struct block *block,
    Type base,
    Type *type,
    String *name)
{
    return parameter_declarator(input, def, block, base, type, name, NULL);
}

static void member_declaration_list(struct preprocessor *input, Type type)
{
    String name;
    struct var expr;
    Type decl_base, decl_type;

    do {
        decl_base = declaration_specifiers(input, NULL, NULL, NULL);
        while (1) {
            name.len = 0;
            declarator(input, NULL, NULL, decl_base, &decl_type, &name);
            if (is_struct_or_union(type) && peek(input).token == ':') {
                if (!is_integer(decl_type)) {
                    error(input, "Unsupported type '%t' for bit-field.", decl_type);
                    exit(1);
                }
                consume(input, ':');
                expr = constant_expression(input);
                if (is_signed(expr.type) && expr.imm.i < 0) {
                    error(input, "Negative width in bit-field.");
                    exit(1);
                }
                type_add_field(input, type, name, decl_type, expr.imm.u);
            } else if (!name.len) {
                if (is_struct_or_union(decl_type)) {
                    type_add_anonymous_member(input, type, decl_type);
                } else {
                    error(input, "Missing name in member declarator.");
                    exit(1);
                }
            } else {
                type_add_member(input, type, name, decl_type);
            }

            if (peek(input).token == ',') {
                consume(input, ',');
            } else break;
        }

        consume(input, ';');
    } while (peek(input).token != '}');
    type_seal(input, type);
}

/*
 * Parse and declare a new struct or union type, or retrieve type from
 * existing symbol; possibly providing a complete definition that will
 * be available for later declarations.
 */
static Type struct_or_union_declaration(
    struct preprocessor *input,
    enum token_type t)
{
    struct symbol *sym = NULL;
    Type type = {0};
    String name;
    enum type kind;

    assert(t == STRUCT || t == UNION);
    kind = (t == STRUCT) ? T_STRUCT : T_UNION;
    if (peek(input).token == IDENTIFIER) {
        name = consume(input, IDENTIFIER).d.string;
        sym = sym_lookup(&ns_tag, name);
        if (!sym) {
            type = type_create(kind);
            sym = sym_add(input, &ns_tag, name, type, SYM_TAG, LINK_NONE);
        } else if (is_integer(sym->type)) {
            error(input, "Tag '%s' was previously declared as enum.",
                str_raw(sym->name));
            exit(1);
        } else if (type_of(sym->type) != kind) {
            error(input, "Tag '%s' was previously declared as %s.",
                str_raw(sym->name),
                (is_struct(sym->type)) ? "struct" : "union");
            exit(1);
        }
        type = sym->type;
        if (peek(input).token == '{' && size_of(type)) {
            error(input, "Redefiniton of '%s'.", str_raw(sym->name));
            exit(1);
        }
    }

    if (peek(input).token == '{') {
        if (!sym) {
            type = type_create(kind);
        }
        next(input);
        member_declaration_list(input, type);
        assert(size_of(type));
        consume(input, '}');
    } else if (!sym) {
        error(input, "Invalid declaration.");
        exit(1);
    }

    return type;
}

static void enumerator_list(struct preprocessor *input)
{
    String name;
    struct var val;
    struct symbol *sym;
    int count = 0;

    consume(input, '{');
    do {
        name = consume(input, IDENTIFIER).d.string;
        if (peek(input).token == '=') {
            consume(input, '=');
            val = constant_expression(input);
            if (!is_integer(val.type)) {
                error(input, "Implicit conversion from non-integer type in enum.");
            }
            count = val.imm.i;
        }
        sym = sym_add(
            input,
            &ns_ident,
            name,
            basic_type__int,
            SYM_CONSTANT,
            LINK_NONE);
        sym->value.constant.i = count++;
        if (peek(input).token != ',')
            break;
        consume(input, ',');
    } while (peek(input).token != '}');
    consume(input, '}');
}

/*
 * Consume enum definition, which represents an int type.
 *
 * Use value.constant as a sentinel to represent definition, checked on
 * lookup to detect duplicate definitions.
 */
static void enum_declaration(struct preprocessor *input)
{
    String name;
    struct token t;
    struct symbol *tag;

    t = peek(input);
    if (t.token == IDENTIFIER) {
        next(input);
        name = t.d.string;
        tag = sym_lookup(&ns_tag, name);
        if (!tag || tag->depth < current_scope_depth(&ns_tag)) {
            tag = sym_add(
                input,
                &ns_tag,
                name,
                basic_type__int,
                SYM_TAG,
                LINK_NONE);
        } else if (!is_integer(tag->type)) {
            error(input, "Tag '%s' was previously defined as aggregate type.",
                str_raw(tag->name));
            exit(1);
        }
        if (peek(input).token == '{') {
            if (tag->value.constant.i) {
                error(input, "Redefiniton of enum '%s'.", str_raw(tag->name));
                exit(1);
            }
            enumerator_list(input);
            tag->value.constant.i = 1;
        }
    } else {
        enumerator_list(input);
    }
}

/*
 * Parse type, qualifiers and storage class. Do not assume int by
 * default, but require at least one type specifier. Storage class is
 * returned as token value, unless the provided pointer is NULL, in
 * which case the input is parsed as specifier-qualifier-list.
 *
 * Type specifiers must be one of the following permutations:
 *
 *     void
 *     char
 *     signed char
 *     unsigned char
 *     short, signed short, short int, or signed short int
 *     unsigned short, or unsigned short int
 *     int, signed, or signed int
 *     unsigned, or unsigned int
 *     long, signed long, long int, or signed long int
 *     unsigned long, or unsigned long int
 *     long long, signed long long, long long int, or
 *     signed long long int
 *     unsigned long long, or unsigned long long int
 *     float
 *     double
 *     long double
 *     _Bool
 *     struct specifier
 *     union specifier
 *     enum specifier
 *     typedef name
 */
INTERNAL Type declaration_specifiers(
    struct preprocessor *input,
    int *storage_class,
    int *is_inline,
    int *is_register)
{
    Type type = {0};
    const Type *tagged;
    struct token tok;
    enum {
        B_NONE,
        B_VOID,
        B_BOOL,
        B_CHAR,
        B_INT,
        B_FLOAT,
        B_DOUBLE,
        B_ENUM,
        B_AGGREGATE
    } base = 0;
    enum {
        M_NONE,
        M_SHORT,
        M_LONG,
        M_LONG_LONG
    } modifier = 0;
    enum {
        S_NONE,
        S_SIGNED,
        S_UNSIGNED
    } sign = 0;
    enum {
        Q_NONE,
        Q_CONST = 1,
        Q_VOLATILE = 2,
        Q_CONST_VOLATILE = Q_CONST | Q_VOLATILE
    } qual = 0;

    if (storage_class) *storage_class = '$';
    if (is_inline) *is_inline = 0;
    if (is_register) *is_register = 0;

    while (1) {
        switch ((tok = peek(input)).token) {
        case VOID:
            if (base || modifier || sign) goto done;
            next(input);
            base = B_VOID;
            break;
        case BOOL:
            if (base || modifier || sign) goto done;
            next(input);
            base = B_BOOL;
            break;
        case CHAR:
            if (base || modifier) goto done;
            next(input);
            base = B_CHAR;
            break;
        case SHORT:
            if (modifier || (base && base != B_INT)) goto done;
            next(input);
            modifier = M_SHORT;
            break;
        case INT:
            if (base) goto done;
            next(input);
            base = B_INT;
            break;
        case LONG:
            if ((base && (base != B_INT || base != B_DOUBLE))
                || (modifier && modifier != M_LONG)) goto done;
            next(input);
            if (modifier == M_LONG) {
                modifier = M_LONG_LONG;
            } else {
                modifier = M_LONG;
            }
            break;
        case SIGNED:
            next(input);
            if (sign == S_SIGNED) {
                error(input, "Duplicate 'signed' specifier.");
            } else if (sign == S_UNSIGNED) {
                error(input, "Conflicting 'signed' and 'unsigned' specifiers.");
            } else {
                sign = S_SIGNED;
            }
            break;
        case UNSIGNED:
            next(input);
            if (sign == S_UNSIGNED) {
                error(input, "Duplicate 'unsigned' specifier.");
            } else if (sign == S_SIGNED) {
                error(input, "Conflicting 'signed' and 'unsigned' specifiers.");
            } else {
                sign = S_UNSIGNED;
            }
            break;
        case FLOAT:
            if (base || modifier || sign) goto done;
            next(input);
            base = B_FLOAT;
            break;
        case DOUBLE:
            if (base || (modifier && modifier != M_LONG) || sign) goto done;
            next(input);
            base = B_DOUBLE;
            break;
        case CONST:
            next(input);
            qual |= Q_CONST;
            break;
        case VOLATILE:
            next(input);
            qual |= Q_VOLATILE;
            break;
        case IDENTIFIER:
            tagged = get_typedef(tok.d.string);
            if (!tagged || base || modifier || sign) goto done;
            next(input);
            type = *tagged;
            base = B_AGGREGATE;
            break;
        case UNION:
        case STRUCT:
            if (base || modifier || sign) goto done;
            next(input);
            type = struct_or_union_declaration(input, tok.token);
            base = B_AGGREGATE;
            break;
        case ENUM:
            if (base || modifier || sign) goto done;
            next(input);
            enum_declaration(input);
            base = B_ENUM;
            break;
        case INLINE:
            next(input);
            if (!is_inline) {
                error(input, "Unexpected 'inline' specifier.");
            } else if (*is_inline) {
                error(input, "Multiple 'inline' specifiers.");
            } else {
                *is_inline = 1;
            }
            break;
        case REGISTER:
            next(input);
            if (!is_register) {
                error(input, "Unexpected 'register' specifier.");
            } else if (*is_register) {
                error(input, "Multiple 'register' specifiers.");
            } else {
                *is_register = 1;
            }
            break;
        case AUTO:
        case STATIC:
        case EXTERN:
        case TYPEDEF:
            next(input);
            if (!storage_class) {
                error(input, "Unexpected storage class in qualifier list.");
            } else if (*storage_class != '$') {
                error(input, "Multiple storage class specifiers.");
            } else {
                *storage_class = tok.token;
            }
            break;
        default:
            goto done;
        }
    }

done:
    switch (base) {
    case B_AGGREGATE:
        break;
    case B_VOID:
        type.type = T_VOID;
        break;
    case B_BOOL:
        type.type = T_BOOL;
        break;
    case B_CHAR:
        type.type = T_CHAR;
        type.is_unsigned = sign == S_UNSIGNED;
        break;
    case B_ENUM:
    case B_NONE:
    case B_INT:
        type.type = T_INT;
        type.is_unsigned = sign == S_UNSIGNED;
        switch (modifier) {
        case M_SHORT:
            type.type = T_SHORT;
            break;
        case M_LONG:
        case M_LONG_LONG:
            type.type = T_LONG;
        default:
            break;
        }
        break;
    case B_FLOAT:
        type.type = T_FLOAT;
        break;
    case B_DOUBLE:
        type.type = T_DOUBLE;
        if (modifier == M_LONG) {
            type.type = T_LDOUBLE;
        }
        break;
    }

    if (qual & Q_CONST)
        type = type_set_const(type);
    if (qual & Q_VOLATILE)
        type = type_set_volatile(type);

    return type;
}

/* Define __func__ as static const char __func__[] = sym->name; */
static void define_builtin__func__(String name)
{
    Type type;
    struct symbol *sym;
    assert(current_scope_depth(&ns_ident) == 1);
    assert(context.standard >= STD_C99);

    /*
     * Just add the symbol directly as a special string value. No
     * explicit assignment reflected in the IR.
     */
    type = type_create_array(basic_type__char, (size_t) name.len + 1);
    sym = sym_add(
        NULL,
        &ns_ident,
        str_init("__func__"),
        type,
        SYM_STRING_VALUE,
        LINK_INTERN);
    sym->value.string = name;
}

/*
 * Parse old-style function definition parameter declarations if present
 * before opening bracket.
 *
 * Verify in the end that all variables have been declared, and add to
 * symbol table parameters that have not been declared old-style.
 * Default to int for parameters that are given without type in the
 * function signature.
 */
static struct block *parameter_declaration_list(
    struct preprocessor *input,
    struct definition *def,
    struct block *block,
    Type type)
{
    int i;
    struct symbol sym = {0};
    struct member *param;

    assert(is_function(type));
    assert(current_scope_depth(&ns_ident) == 1);
    assert(!def->symbol);

    sym.type = type;
    def->symbol = &sym;
    while (peek(input).token != '{') {
        block = declaration(input, def, block);
    }

    def->symbol = NULL;
    for (i = 0; i < nmembers(type); ++i) {
        param = get_member(type, i);
        if (!param->name.len) {
            error(input, "Missing parameter name at position %d.", i + 1);
            exit(1);
        }

        assert(!param->sym);
        param->sym = sym_lookup(&ns_ident, param->name);
        if (!param->sym || param->sym->depth != 1) {
            assert(is_type_placeholder(param->type));
            param->type = basic_type__int;
            param->sym = sym_add(input, &ns_ident,
                param->name,
                param->type,
                SYM_DEFINITION,
                LINK_NONE);
        }
    }

    type_seal(input, type);
    assert(is_complete(type));
    return block;
}

/* Add function parameters to scope. */
static struct block *make_parameters_visible(
    struct preprocessor *input,
    struct definition *def,
    struct block *block)
{
    int i;
    struct member *param;

    assert(def->symbol);
    assert(is_function(def->symbol->type));
    assert(current_scope_depth(&ns_ident) == 1);

    for (i = 0; i < nmembers(def->symbol->type); ++i) {
        param = get_member(def->symbol->type, i);
        if (!param->name.len) {
            error(input, "Missing parameter at position %d.", i + 1);
            exit(1);
        }

        assert(param->sym);
        assert(param->sym->depth == 1);
        assert(!is_type_placeholder(param->type));
        assert(!is_array(param->type));
        sym_make_visible(&ns_ident, param->sym);
        array_push_back(&def->params, param->sym);
    }

    return block;
}

INTERNAL struct block *declare_vla(
    struct preprocessor *input,
    struct definition *def,
    struct block *block,
    struct symbol *sym)
{
    struct symbol *addr;

    assert(is_vla(sym->type));
    addr = sym_create_temporary(type_create_pointer(type_next(sym->type)));
    array_push_back(&def->locals, addr);
    sym->value.vla_address = addr;
    eval_vla_alloc(input, def, block, sym);
    return block;
}

/*
 * Parse declaration, possibly with initializer. New symbols are added
 * to the symbol table.
 *
 * Cover external declarations, functions, and local declarations
 * (with optional initialization code) inside functions.
 */
INTERNAL struct block *init_declarator(
    struct preprocessor *input,
    struct definition *def,
    struct block *parent,
    Type base,
    enum symtype symtype,
    enum linkage linkage)
{
    Type type;
    String name = {0};
    struct symbol *sym;
    const struct member *param;

    if (linkage == LINK_INTERN && current_scope_depth(&ns_ident) != 0) {
        declarator(input, def, cfg_block_init(def), base, &type, &name);
    } else {
        parent = declarator(input, def, parent, base, &type, &name);
    }

    if (!name.len) {
        return parent;
    }

    if (symtype == SYM_TYPEDEF) {
        /* */
    } else if (is_function(type)) {
        symtype = SYM_DECLARATION;
        linkage = (linkage == LINK_NONE) ? LINK_EXTERN : linkage;
        if (linkage == LINK_INTERN && current_scope_depth(&ns_ident)) {
            error(input, "Cannot declare static function in block scope.");
            exit(1);
        }
    } else if (is_variably_modified(type)) {
        if (current_scope_depth(&ns_ident) == 0) {
            error(input, "Invalid variably modified type at file scope.");
            exit(1);
        } else if (linkage != LINK_NONE
            && !(is_pointer(type) && linkage == LINK_INTERN))
        {
            error(input, "Invalid linkage for block scoped variably modified type.");
            exit(1);
        }
    }

    if (is_function(type) && !is_complete(type) && peek(input).token != ';') {
        push_scope(&ns_ident);
        parent = parameter_declaration_list(input, def, parent, type);
        pop_scope(&ns_ident);
    }

    sym = sym_add(input, &ns_ident, name, type, symtype, linkage);
    switch (current_scope_depth(&ns_ident)) {
    case 0: break;
    case 1: /* Parameters from old-style function definitions. */
        assert(def->symbol);
        param = find_type_member(def->symbol->type, name, NULL);
        if (is_array(type)) {
            sym->type = type_create_pointer(type_next(type));
        }
        if (param && is_type_placeholder(param->type)) {
            ((struct member *) param)->type = sym->type;
        } else {
            error(input, "Invalid parameter declaration of %s.", str_raw(name));
            exit(1);
        }
        break;
    default:
        if (symtype == SYM_DEFINITION) {
            assert(linkage == LINK_NONE);
            array_push_back(&def->locals, sym);
            if (is_vla(type)) {
                parent = declare_vla(input, def, parent, sym);
            }
        }
        break;
    }

    switch (peek(input).token) {
    case '=':
        if (sym->symtype == SYM_DECLARATION) {
            error(input, "Extern symbol '%s' cannot be initialized.",
                str_raw(sym->name));
            exit(1);
        }
        if (!sym->depth && sym->symtype == SYM_DEFINITION) {
            error(input, "Symbol '%s' was already defined.", str_raw(sym->name));
            exit(1);
        }
        if (is_vla(sym->type)) {
            error(input, "Variable length array cannot be initialized.");
            exit(1);
        }
        consume(input, '=');
        sym->symtype = SYM_DEFINITION;
        parent = initializer(input, def, parent, sym);
        assert(size_of(sym->type) > 0);
        if (sym->linkage != LINK_NONE) {
            cfg_define(def, sym);
        }
        break;
    case IDENTIFIER:
    case FIRST(type_specifier):
    case FIRST(type_qualifier):
    case REGISTER:
    case '{':
        assert(sym->linkage != LINK_NONE);
        if (is_function(sym->type)) {
            sym->symtype = SYM_DEFINITION;
            cfg_define(def, sym);
            push_scope(&ns_label);
            push_scope(&ns_ident);
            parent = make_parameters_visible(input, def, parent);
            if (context.standard >= STD_C99) {
                define_builtin__func__(sym->name);
            }
            parent = block(input, def, parent);
            pop_scope(&ns_label);
            pop_scope(&ns_ident);
            return parent;
        }
    default:
        break;
    }

    if (linkage == LINK_INTERN
        || (is_function(sym->type) && sym->symtype != SYM_DEFINITION))
    {
        type_clean_prototype(sym->type);
    }

    return parent;
}

static void static_assertion(struct preprocessor *input)
{
    struct var val;
    String message;

    consume(input, STATIC_ASSERT);
    consume(input, '(');

    val = constant_expression(input);
    consume(input, ',');
    message = consume(input, STRING).d.string;

    if (val.kind != IMMEDIATE || !is_integer(val.type)) {
        error(input, "Expression in static assertion must be an integer constant.");
        exit(1);
    }

    if (val.imm.i == 0) {
        error(input, str_raw(message));
        exit(1);
    }

    consume(input, ')');
}

/*
 * Parse a declaration list, beginning with a base set of specifiers,
 * followed by a list of declarators.
 *
 * Each new global declaration is assigned a clean 'struct definition'
 * object, which might get filled with initialization code, or the body
 * of a function.
 *
 * Terminate on hitting a function definition, otherwise read until the
 * end of statement.
 */
INTERNAL struct block *declaration(
    struct preprocessor *input,
    struct definition *def,
    struct block *parent)
{
    Type base;
    enum symtype symtype;
    enum linkage linkage;
    struct definition *decl;
    int storage_class, is_inline, is_register;

    if (peek(input).token == STATIC_ASSERT) {
        static_assertion(input);
        consume(input, ';');
        return parent;
    }

    base = declaration_specifiers(
        input,
        &storage_class,
        &is_inline,
        &is_register);

    switch (storage_class) {
    case EXTERN:
        symtype = SYM_DECLARATION;
        linkage = LINK_EXTERN;
        break;
    case STATIC:
        symtype = SYM_TENTATIVE;
        linkage = LINK_INTERN;
        break;
    case TYPEDEF:
        symtype = SYM_TYPEDEF;
        linkage = LINK_NONE;
        break;
    default:
        if (!current_scope_depth(&ns_ident)) {
            symtype = SYM_TENTATIVE;
            linkage = LINK_EXTERN;
        } else {
            symtype = SYM_DEFINITION;
            linkage = LINK_NONE;
        }
        break;
    }

    switch (peek(input).token) {
    case '*':
    case '(':
    case IDENTIFIER:
        break;
    default:
        consume(input, ';');
        return parent;
    }

    while (1) {
        if (linkage == LINK_INTERN || linkage == LINK_EXTERN) {
            decl = cfg_init();
            init_declarator(input, decl, decl->body, base, symtype, linkage);
            if (!decl->symbol) {
                cfg_discard(decl);
            } else if (is_function(decl->symbol->type)) {
                return parent;
            }
        } else {
            parent = init_declarator(input, def, parent, base, symtype, linkage);
        }

        if (peek(input).token == ',') {
            next(input);
        } else break;
    }

    consume(input, ';');
    return parent;
}
