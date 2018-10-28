#if !AMALGAMATION
# define INTERNAL
# define EXTERNAL extern
#endif
#include "declaration.h"
#include "eval.h"
#include "expression.h"
#include "initializer.h"
#include "parse.h"
#include "symtab.h"
#include "typetree.h"
#include <lacc/context.h>
#include <lacc/token.h>

#include <assert.h>

static struct block *cast_expression(
    struct preprocessor *input,
    struct definition *def,
    struct block *block);

static const struct symbol *find_symbol(struct preprocessor *input, String name)
{
    const struct symbol *sym = sym_lookup(&ns_ident, name);
    if (!sym) {
        error(input, "Undefined symbol '%s'.", str_raw(name));
        exit(1);
    }

    return sym;
}

/*
 * Parse call to builtin symbol __builtin_va_start, which is the result
 * of calling va_start(arg, s). Return type depends on second input
 * argument.
 */
static struct block *parse__builtin_va_start(
    struct preprocessor *input,
    struct definition *def,
    struct block *block)
{
    Type type;
    const struct member *mb;
    const struct symbol *sym;
    struct token param;

    consume(input, '(');
    block = assignment_expression(input, def, block);
    consume(input, ',');
    param = consume(input, IDENTIFIER);

    sym = find_symbol(input, param.d.string);
    type = def->symbol->type;
    if (!is_vararg(type)) {
        error(input, "Function must be vararg to use va_start.");
        exit(1);
    }

    mb = get_member(type, nmembers(type) - 1);
    if (str_cmp(mb->name, sym->name) || sym->depth != 1) {
        error(input, "Expected last function argument %s as va_start argument.",
            str_raw(mb->name));
        exit(1);
    }

    consume(input, ')');
    eval__builtin_va_start(block, block->expr);
    return block;
}

/*
 * Parse call to builtin symbol __builtin_va_arg, which is the result of
 * calling va_arg(arg, T). Return type depends on second input argument.
 */
static struct block *parse__builtin_va_arg(
    struct preprocessor *input,
    struct definition *def,
    struct block *block)
{
    struct var value;
    Type type;

    consume(input, '(');
    block = assignment_expression(input, def, block);
    value = eval(def, block, block->expr);
    consume(input, ',');
    type = declaration_specifiers(input, NULL, NULL, NULL);
    if (peek(input).token != ')') {
        block = declarator(input, def, block, type, &type, NULL);
    }

    consume(input, ')');
    block->expr = eval_expr(input, def, block, IR_OP_VA_ARG, value, type);
    return block;
}

/*
 * Implement alloca as a normal VLA.
 *
 *     void *ptr = alloca(n + 1);
 *
 * is translated to
 *
 *     size_t len = n + 1;
 *     char sym[len];
 *     void *ptr = (void *) sym;
 */
static struct block *parse__builtin_alloca(
    struct preprocessor *input,
    struct definition *def,
    struct block *block)
{
    struct var t1;
    struct symbol *sym;

    consume(input, '(');
    block = assignment_expression(input, def, block);
    consume(input, ')');

    t1 = create_var(def, basic_type__unsigned_long);
    eval_assign(input, def, block, t1, block->expr);

    sym = sym_create_temporary(type_create_vla(basic_type__char, t1.symbol));
    array_push_back(&def->locals, sym);

    block = declare_vla(input, def, block, sym);
    block->expr = eval_expr(input, def, block, IR_OP_CAST, var_direct(sym),
        type_create_pointer(basic_type__void));
    return block;
}

/*
 * Special handling for builtin pseudo functions. These are expected to
 * behave as macros, thus should be no problem parsing as function call
 * in primary expression. Constructs like (va_arg)(args, int) will not
 * work with this scheme.
 *
 * String constants become IMMEDIATE of type [] char, with a reference
 * to the new symbol containing the string literal. Decays into char *
 * on evaluation.
 */
static struct block *primary_expression(
    struct preprocessor *input,
    struct definition *def,
    struct block *block)
{
    const struct symbol *sym;
    struct token tok;

    switch ((tok = next(input)).token) {
    case IDENTIFIER:
        sym = find_symbol(input, tok.d.string);
        if (!strcmp("__builtin_va_start", str_raw(sym->name))) {
            block = parse__builtin_va_start(input, def, block);
        } else if (!strcmp("__builtin_va_arg", str_raw(sym->name))) {
            block = parse__builtin_va_arg(input, def, block);
        } else if (!strcmp("__builtin_alloca", str_raw(sym->name))) {
            block = parse__builtin_alloca(input, def, block);
        } else {
            block->expr = as_expr(var_direct(sym));
        }
        break;
    case NUMBER:
        block->expr = as_expr(var_numeric(tok.type, tok.d.val));
        assert(is_identity(block->expr));
        break;
    case '(':
        block = expression(input, def, block);
        consume(input, ')');
        break;
    case STRING:
        sym = sym_create_string(tok.d.string);
        block->expr = as_expr(var_direct(sym));
        assert(is_identity(block->expr));
        assert(block->expr.l.kind == IMMEDIATE);
        break;
    default:
        error(input, "Unexpected '%s', not a valid primary expression.",
            str_raw(tok.d.string));
        exit(1);
    }

    return block;
}

typedef array_of(struct expression) ExprArray;

/*
 * Need to buffer parameter expressions before each function call, and
 * since calls can be nested, the same buffer cannot be used for all.
 */
static array_of(ExprArray *) args;
static unsigned max_depth;

INTERNAL void clear_argument_lists(void)
{
    int i;
    ExprArray *a;

    for (i = 0; i < max_depth; ++i) {
        a = array_get(&args, i);
        array_clear(a);
        free(a);
    }

    array_clear(&args);
}

static ExprArray *push_argument_list(void)
{
    unsigned len;
    ExprArray *list;

    len = array_len(&args);
    if (len == max_depth) {
        list = calloc(1, sizeof(*list));
        array_push_back(&args, list);
        max_depth = len + 1;
    } else {
        list = array_get(&args, len);
        args.length += 1;
    }

    return list;
}

static void pop_argument_list(void)
{
    ExprArray *list;
    assert(args.length);

    args.length -= 1;
    list = array_get(&args, array_len(&args));
    array_empty(list);
}

static struct block *postfix(
    struct preprocessor *input,
    struct definition *def,
    struct block *block)
{
    struct expression root;
    struct var value, copy;
    const struct member *mbr;
    struct token tok;
    int i;
    Type type;
    ExprArray *args;

    root = block->expr;

    while (1) {
        switch ((tok = peek(input)).token) {
        case '[':
            do {
                /*
                 * Evaluate a[b] = *(a + b). The semantics of pointer
                 * arithmetic takes care of multiplying b with the
                 * correct width.
                 */
                consume(input, '[');
                value = eval(def, block, block->expr);
                block = expression(input, def, block);
                block->expr =
                    eval_expr(input, def, block, IR_OP_ADD, value,
                        eval(def, block, block->expr));
                block->expr =
                    as_expr(
                        eval_deref(input, def, block,
                            eval(def, block, block->expr)));
                consume(input, ']');
            } while (peek(input).token == '[');
            root = block->expr;
            break;
        case '(':
            type = root.type;
            if (is_pointer(root.type) && is_function(type_deref(root.type))) {
                type = type_deref(root.type);
            } else if (!is_function(root.type)) {
                error(input, "Expression must have type pointer to function, was %t.",
                    type);
                exit(1);
            }
            consume(input, '(');
            args = push_argument_list();
            for (i = 0; i < nmembers(type); ++i) {
                if (peek(input).token == ')') {
                    error(input, "Too few arguments, expected %d but got %d.",
                        nmembers(type), i);
                    exit(1);
                }
                mbr = get_member(type, i);
                block = assignment_expression(input, def, block);
                if (!type_equal(block->expr.type, mbr->type)) {
                    value = eval(def, block, block->expr);
                    block->expr =
                        eval_expr(input, def, block, IR_OP_CAST, value, mbr->type);
                }
                block->expr = eval_param(input, def, block, block->expr);
                array_push_back(args, block->expr);
                if (i < nmembers(type) - 1) {
                    consume(input, ',');
                }
            }
            while (is_vararg(type) && peek(input).token != ')') {
                consume(input, ',');
                block = assignment_expression(input, def, block);
                if (is_float(block->expr.type)) {
                    /*
                     * Single-precision arguments to vararg function are
                     * automatically promoted to double.
                     */
                    value = eval(def, block, block->expr);
                    block->expr = eval_expr(input, def, block, IR_OP_CAST,
                        value, basic_type__double);
                }
                block->expr = eval_param(input, def, block, block->expr);
                array_push_back(args, block->expr);
                i++;
            }
            consume(input, ')');
            for (i = 0; i < array_len(args); ++i) {
                param(block, array_get(args, i));
            }
            value = eval(def, block, root);
            block->expr = eval_expr(input, def, block, IR_OP_CALL, value);
            root = block->expr;
            pop_argument_list();
            break;
        case '.':
            consume(input, '.');
            tok = consume(input, IDENTIFIER);
            mbr = find_type_member(root.type, tok.d.string, NULL);
            if (!mbr) {
                error(input, "Invalid access, no member named '%s'.",
                    str_raw(tok.d.string));
                exit(1);
            }
            value = eval(def, block, root);
            value.type = mbr->type;
            value.field_width = mbr->field_width;
            value.field_offset = mbr->field_offset;
            value.offset += mbr->offset;
            block->expr = as_expr(value);
            root = block->expr;
            break;
        case ARROW:
            consume(input, ARROW);
            tok = consume(input, IDENTIFIER);
            value = eval_deref(input, def, block, eval(def, block, root));
            if (is_struct_or_union(value.type)) {
                mbr = find_type_member(value.type, tok.d.string, NULL);
                if (!mbr) {
                    error(input, "Invalid access, %t has no member named '%s'.",
                        value.type, str_raw(tok.d.string));
                    exit(1);
                }
                value.type = mbr->type;
                value.field_width = mbr->field_width;
                value.field_offset = mbr->field_offset;
                value.offset += mbr->offset;
                block->expr = as_expr(value);
                root = block->expr;
            } else {
                error(input, "Invalid member access to type %t.", root.type);
                exit(1);
            }
            break;
        case INCREMENT:
            consume(input, INCREMENT);
            value = eval(def, block, root);
            copy = eval_copy(input, def, block, value);
            root = eval_expr(input, def, block, IR_OP_ADD, value, var_int(1));
            eval_assign(input, def, block, value, root);
            block->expr = as_expr(copy);
            root = block->expr;
            break;
        case DECREMENT:
            consume(input, DECREMENT);
            value = eval(def, block, root);
            copy = eval_copy(input, def, block, value);
            root = eval_expr(input, def, block, IR_OP_SUB, value, var_int(1));
            eval_assign(input, def, block, value, root);
            block->expr = as_expr(copy);
            root = block->expr;
            break;
        default:
            block->expr = root;
            return block;
        }
    }
}

static struct block *postfix_expression(
    struct preprocessor *input,
    struct definition *def,
    struct block *block)
{
    const struct symbol *sym;
    struct token tok;
    Type type;

    /*
     * Special case for function calls directly on an identifier which
     * is not declared. Add a declaration like 'extern int foo()' to the
     * current scope.
     */
    if (context.standard == STD_C89) {
        tok = peek(input);
        if (tok.token == IDENTIFIER && peekn(input, 2).token == '(') {
            sym = sym_lookup(&ns_ident, tok.d.string);
            if (!sym) {
                type = type_create_function(basic_type__int);
                sym_add(input, &ns_ident, tok.d.string, type,
                    SYM_DECLARATION, LINK_EXTERN);
            }
        }
    }

    block = primary_expression(input, def, block);
    return postfix(input, def, block);
}

static struct block *unary_expression(
    struct preprocessor *input,
    struct definition *def,
    struct block *block)
{
    struct var value;
    struct block *head, *tail;
    const struct symbol *sym;
    Type type;

    switch (peek(input).token) {
    case '&':
        consume(input, '&');
        block = cast_expression(input, def, block);
        value = eval(def, block, block->expr);
        block->expr = as_expr(eval_addr(input, def, block, value));
        break;
    case '*':
        consume(input, '*');
        block = cast_expression(input, def, block);
        value = eval(def, block, block->expr);
        block->expr = as_expr(eval_deref(input, def, block, value));
        break;
    case '!':
        consume(input, '!');
        block = cast_expression(input, def, block);
        switch (block->expr.op) {
        case IR_OP_EQ:
            block->expr.op = IR_OP_NE;
            break;
        case IR_OP_NE:
            block->expr.op = IR_OP_EQ;
            break;
        case IR_OP_GE:
            block->expr.op = IR_OP_GT;
            value = block->expr.l;
            block->expr.l = block->expr.r;
            block->expr.r = value;
            break;
        case IR_OP_GT:
            block->expr.op = IR_OP_GE;
            value = block->expr.l;
            block->expr.l = block->expr.r;
            block->expr.r = value;
            break;
        default:
            value = eval(def, block, block->expr);
            block->expr = eval_expr(input, def, block, IR_OP_EQ, var_int(0), value);
            break;
        }
        break;
    case '~':
        consume(input, '~');
        block = cast_expression(input, def, block);
        value = eval(def, block, block->expr);
        block->expr = eval_expr(input, def, block, IR_OP_NOT, value);
        break;
    case '+':
        consume(input, '+');
        block = cast_expression(input, def, block);
        value = eval(def, block, block->expr);
        block->expr = eval_unary_plus(input, value);
        break;
    case '-':
        consume(input, '-');
        block = cast_expression(input, def, block);
        value = eval(def, block, block->expr);
        block->expr = eval_expr(input, def, block, IR_OP_NEG, value);
        break;
    case SIZEOF:
        consume(input, SIZEOF);
        if (peek(input).token == '(') {
            switch (peekn(input, 2).token) {
            case IDENTIFIER:
                sym = sym_lookup(&ns_ident, peekn(input, 2).d.string);
                if (!sym || sym->symtype != SYM_TYPEDEF)
                    goto exprsize;;
            case FIRST(type_name):
                consume(input, '(');
                type = declaration_specifiers(input, NULL, NULL, NULL);
                if (peek(input).token != ')') {
                    block = declarator(input, def, block, type, &type, NULL);
                }
                consume(input, ')');
                break;
            default: goto exprsize;
            }
        } else {
exprsize:   head = cfg_block_init(def);
            tail = unary_expression(input, def, head);
            type = tail->expr.type;
        }
        if (is_complete(type)) {
            if (is_vla(type)) {
                block->expr = eval_vla_size(input, def, block, type);
            } else {
                value = imm_unsigned(basic_type__unsigned_long, size_of(type));
                block->expr = as_expr(value);
            }
        } else {
            error(input, "Cannot apply 'sizeof' to incomplete type.");
            exit(1);
        }
        break;
    case ALIGNOF:
        next(input);
        consume(input, '(');
        type = declaration_specifiers(input, NULL, NULL, NULL);
        if (peek(input).token != ')') {
            block = declarator(input, def, block, type, &type, NULL);
        }
        if (is_function(type)) {
            error(input, "Cannot apply '_Alignof' to function type.");
        }
        if (!size_of(type)) {
            error(input, "Cannot apply '_Alignof' to incomplete type.");
        }
        value = imm_unsigned(basic_type__unsigned_long, type_alignment(type));
        block->expr = as_expr(value);
        consume(input, ')');
        break;
    case INCREMENT:
        consume(input, INCREMENT);
        block = unary_expression(input, def, block);
        value = eval(def, block, block->expr);
        block->expr =
            eval_expr(input, def, block, IR_OP_ADD, value, var_int(1));
        block->expr = as_expr(
            eval_assign(input, def, block, value, block->expr));
        break;
    case DECREMENT:
        consume(input, DECREMENT);
        block = unary_expression(input, def, block);
        value = eval(def, block, block->expr);
        block->expr =
            eval_expr(input, def, block, IR_OP_SUB, value, var_int(1));
        block->expr = as_expr(
            eval_assign(input, def, block, value, block->expr));
        break;
    default:
        block = postfix_expression(input, def, block);
        break;
    }

    return block;
}

static struct block *compound_literal(
    struct preprocessor *input,
    struct definition *def,
    struct block *block,
    Type type)
{
    struct var var;
    struct symbol *sym;

    sym = sym_create_unnamed(type);
    if (sym->linkage == LINK_INTERN) {
        def = cfg_init();
        initializer(input, def, def->body, sym);
        cfg_define(def, sym);
    } else {
        array_push_back(&def->locals, sym);
        block = initializer(input, def, block, sym);
    }

    var = var_direct(sym);
    block->expr = as_expr(var);
    return block;
}

/*
 * This rule needs two lookahead; to see beyond the initial parenthesis
 * whether it is actually a cast or an expression.
 *
 * Also handle compound literals, which are really postfix expressions,
 * but have the same prefix as a cast expression.
 */
static struct block *cast_expression(
    struct preprocessor *input,
    struct definition *def,
    struct block *block)
{
    struct var value;
    struct token tok;
    struct symbol *sym;
    Type type;

    if (peek(input).token == '(') {
        tok = peekn(input, 2);
        switch (tok.token) {
        case IDENTIFIER:
            sym = sym_lookup(&ns_ident, tok.d.string);
            if (!sym || sym->symtype != SYM_TYPEDEF)
                break;
        case FIRST(type_name):
            next(input);
            type = declaration_specifiers(input, NULL, NULL, NULL);
            if (peek(input).token != ')') {
                block = declarator(input, def, block, type, &type, NULL);
            }
            consume(input, ')');
            if (peek(input).token == '{') {
                block = compound_literal(input, def, block, type);
                return postfix(input, def, block);
            } else {
                block = cast_expression(input, def, block);
                value = eval(def, block, block->expr);
                block->expr =
                    eval_expr(input, def, block, IR_OP_CAST, value, type);
                return block;
            }
        default:
            break;
        }
    }

    return unary_expression(input, def, block);
}

static struct block *multiplicative_expression(
    struct preprocessor *input,
    struct definition *def,
    struct block *block)
{
    struct var value;
    struct token t;

    block = cast_expression(input, def, block);
    while (1) {
        t = peek(input);
        if (t.token == '*') {
            consume(input, '*');
            value = eval(def, block, block->expr);
            block = cast_expression(input, def, block);
            block->expr = eval_expr(input, def, block, IR_OP_MUL, value,
                eval(def, block, block->expr));
        } else if (t.token == '/') {
            consume(input, '/');
            value = eval(def, block, block->expr);
            block = cast_expression(input, def, block);
            block->expr = eval_expr(input, def, block, IR_OP_DIV, value,
                eval(def, block, block->expr));
        } else if (t.token == '%') {
            consume(input, '%');
            value = eval(def, block, block->expr);
            block = cast_expression(input, def, block);
            block->expr = eval_expr(input, def, block, IR_OP_MOD, value,
                eval(def, block, block->expr));
        } else break;
    }

    return block;
}

static struct block *additive_expression(
    struct preprocessor *input,
    struct definition *def,
    struct block *block)
{
    struct var value;
    struct token t;

    block = multiplicative_expression(input, def, block);
    while (1) {
        t = peek(input);
        if (t.token == '+') {
            consume(input, '+');
            value = eval(def, block, block->expr);
            block = multiplicative_expression(input, def, block);
            block->expr = eval_expr(input, def, block, IR_OP_ADD, value,
                eval(def, block, block->expr));
        } else if (t.token == '-') {
            consume(input, '-');
            value = eval(def, block, block->expr);
            block = multiplicative_expression(input, def, block);
            block->expr = eval_expr(input, def, block, IR_OP_SUB, value,
                eval(def, block, block->expr));
        } else break;
    }

    return block;
}

static struct block *shift_expression(
    struct preprocessor *input,
    struct definition *def,
    struct block *block)
{
    struct var value;
    struct token t;

    block = additive_expression(input, def, block);
    while (1) {
        t = peek(input);
        if (t.token == LSHIFT) {
            consume(input, LSHIFT);
            value = eval(def, block, block->expr);
            block = additive_expression(input, def, block);
            block->expr = eval_expr(input, def, block, IR_OP_SHL, value,
                eval(def, block, block->expr));
        } else if (t.token == RSHIFT) {
            consume(input, RSHIFT);
            value = eval(def, block, block->expr);
            block = additive_expression(input, def, block);
            block->expr = eval_expr(input, def, block, IR_OP_SHR, value,
                eval(def, block, block->expr));
        } else break;
    }

    return block;
}

static struct block *relational_expression(
    struct preprocessor *input,
    struct definition *def,
    struct block *block)
{
    struct var value;

    block = shift_expression(input, def, block);
    while (1) {
        switch (peek(input).token) {
        case '<':
            consume(input, '<');
            value = eval(def, block, block->expr);
            block = shift_expression(input, def, block);
            block->expr = eval_expr(input, def, block, IR_OP_GT,
                eval(def, block, block->expr), value);
            break;
        case '>':
            consume(input, '>');
            value = eval(def, block, block->expr);
            block = shift_expression(input, def, block);
            block->expr = eval_expr(input, def, block, IR_OP_GT,
                value, eval(def, block, block->expr));
            break;
        case LEQ:
            consume(input, LEQ);
            value = eval(def, block, block->expr);
            block = shift_expression(input, def, block);
            block->expr = eval_expr(input, def, block, IR_OP_GE,
                eval(def, block, block->expr), value);
            break;
        case GEQ:
            consume(input, GEQ);
            value = eval(def, block, block->expr);
            block = shift_expression(input, def, block);
            block->expr = eval_expr(input, def, block, IR_OP_GE,
                value, eval(def, block, block->expr));
            break;
        default:
            return block;
        }
    }
}

static struct block *equality_expression(
    struct preprocessor *input,
    struct definition *def,
    struct block *block)
{
    enum optype op;
    struct var value;
    struct token t;

    block = relational_expression(input, def, block);
    while (1) {
        t = peek(input);
        if (t.token == EQ) {
            consume(input, EQ);
            op = IR_OP_EQ;
        } else if (t.token == NEQ) {
            consume(input, NEQ);
            op = IR_OP_NE;
        } else break;
        value = eval(def, block, block->expr);
        block = relational_expression(input, def, block);
        block->expr =
            eval_expr(input, def, block, op, value,
                eval(def, block, block->expr));
    }

    return block;
}

static struct block *and_expression(
    struct preprocessor *input,
    struct definition *def,
    struct block *block)
{
    struct var value;

    block = equality_expression(input, def, block);
    while (peek(input).token == '&') {
        consume(input, '&');
        value = eval(def, block, block->expr);
        block = equality_expression(input, def, block);
        block->expr = eval_expr(input, def, block, IR_OP_AND, value,
            eval(def, block, block->expr));
    }

    return block;
}

static struct block *exclusive_or_expression(
    struct preprocessor *input,
    struct definition *def,
    struct block *block)
{
    struct var value;

    block = and_expression(input, def, block);
    while (peek(input).token == '^') {
        consume(input, '^');
        value = eval(def, block, block->expr);
        block = and_expression(input, def, block);
        block->expr = eval_expr(input, def, block, IR_OP_XOR, value,
            eval(def, block, block->expr));
    }

    return block;
}

static struct block *inclusive_or_expression(
    struct preprocessor *input,
    struct definition *def,
    struct block *block)
{
    struct var value;

    block = exclusive_or_expression(input, def, block);
    while (peek(input).token == '|') {
        consume(input, '|');
        value = eval(def, block, block->expr);
        block = exclusive_or_expression(input, def, block);
        block->expr = eval_expr(input, def, block, IR_OP_OR, value,
            eval(def, block, block->expr));
    }

    return block;
}

static struct block *logical_and_expression(
    struct preprocessor *input,
    struct definition *def,
    struct block *block)
{
    struct block *right;

    block = inclusive_or_expression(input, def, block);
    if (peek(input).token == LOGICAL_AND) {
        consume(input, LOGICAL_AND);
        right = cfg_block_init(def);
        block = eval_logical_and(
            input, def, block, right,
            logical_and_expression(input, def, right));
    }

    return block;
}

static struct block *logical_or_expression(
    struct preprocessor *input,
    struct definition *def,
    struct block *block)
{
    struct block *right;

    block = logical_and_expression(input, def, block);
    if (peek(input).token == LOGICAL_OR) {
        consume(input, LOGICAL_OR);
        right = cfg_block_init(def);
        block = eval_logical_or(
            input, def, block, right,
            logical_or_expression(input, def, right));
    }

    return block;
}

static struct block *conditional_expression(
    struct preprocessor *input,
    struct definition *def,
    struct block *block)
{
    struct var temp;
    struct block *left, *right;
    Type type;

    block = logical_or_expression(input, def, block);
    if (peek(input).token != '?') {
        return block;
    }

    if (!is_scalar(block->expr.type)) {
        error(input, "Conditional must be of scalar type.");
        exit(1);
    }

    next(input);
    if (is_immediate(block->expr)) {
        if (is_immediate_true(block->expr)) {
            left = block = expression(input, def, block);
            consume(input, ':');
            right = cfg_block_init(def);
            right = conditional_expression(input, def, right);
        } else {
            assert(is_immediate_false(block->expr));
            left = cfg_block_init(def);
            left = expression(input, def, left);
            consume(input, ':');
            right = block = conditional_expression(input, def, block);
        }

        type = eval_conditional(input, def, left, right);
        if (is_void(type)) {
            block->expr = as_expr(var_void());
        } else {
            block->expr = eval_expr(input, def, block, IR_OP_CAST,
                eval(def, block, block->expr), type);
        }
    } else {
        left = cfg_block_init(def);
        right = cfg_block_init(def);
        block->jump[0] = right;
        block->jump[1] = left;
        block = cfg_block_init(def);
        left = expression(input, def, left);
        left->jump[0] = block;
        consume(input, ':');
        right = conditional_expression(input, def, right);
        right->jump[0] = block;
        type = eval_conditional(input, def, left, right);
        if (is_void(type)) {
            block->expr = as_expr(var_void());
        } else {
            temp = create_var(def, type);
            left->expr = as_expr(eval_assign(input, def, left, temp, left->expr));
            right->expr = as_expr(eval_assign(input, def, right, temp, right->expr));
            block->expr = as_expr(temp);
        }
    }

    return block;
}

INTERNAL struct block *assignment_expression(
    struct preprocessor *input,
    struct definition *def,
    struct block *block)
{
    enum optype op = IR_OP_CAST;
    struct var target, value;

    block = conditional_expression(input, def, block);
    switch (peek(input).token) {
    case '=':
        consume(input, '=');
        break;
    case MUL_ASSIGN:
        consume(input, MUL_ASSIGN);
        op = IR_OP_MUL;
        break;
    case DIV_ASSIGN:
        consume(input, DIV_ASSIGN);
        op = IR_OP_DIV;
        break;
    case MOD_ASSIGN:
        consume(input, MOD_ASSIGN);
        op = IR_OP_MOD;
        break;
    case PLUS_ASSIGN:
        consume(input, PLUS_ASSIGN);
        op = IR_OP_ADD;
        break;
    case MINUS_ASSIGN:
        consume(input, MINUS_ASSIGN);
        op = IR_OP_SUB;
        break;
    case AND_ASSIGN:
        consume(input, AND_ASSIGN);
        op = IR_OP_AND;
        break;
    case OR_ASSIGN:
        consume(input, OR_ASSIGN);
        op = IR_OP_OR;
        break;
    case XOR_ASSIGN:
        consume(input, XOR_ASSIGN);
        op = IR_OP_XOR;
        break;
    case RSHIFT_ASSIGN:
        consume(input, RSHIFT_ASSIGN);
        op = IR_OP_SHR;
        break;
    case LSHIFT_ASSIGN:
        consume(input, LSHIFT_ASSIGN);
        op = IR_OP_SHL;
        break;
    default:
        return block;
    }

    target = eval(def, block, block->expr);
    block = assignment_expression(input, def, block);
    if (op != IR_OP_CAST) {
        value = eval(def, block, block->expr);
        block->expr = eval_expr(input, def, block, op, target, value);
    }

    value = eval_assign(input, def, block, target, block->expr);
    block->expr = as_expr(value);
    return block;
}

INTERNAL struct var constant_expression(struct preprocessor *input)
{
    struct block
        *head = cfg_block_init(NULL),
        *tail;

    tail = conditional_expression(input, NULL, head);
    if (tail != head || !is_immediate(tail->expr)) {
        error(input, "Constant expression must be computable at compile time.");
        exit(1);
    }

    return eval(NULL, tail, tail->expr);
}

INTERNAL struct block *expression(
    struct preprocessor *input,
    struct definition *def,
    struct block *block)
{
    block = assignment_expression(input, def, block);
    while (peek(input).token == ',') {
        consume(input, ',');
        if (has_side_effects(block->expr)) {
            eval(def, block, block->expr);
        }
        block = assignment_expression(input, def, block);
    }

    return block;
}
