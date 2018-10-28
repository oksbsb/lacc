#if !AMALGAMATION
# define INTERNAL
# define EXTERNAL extern
#endif
#include "statement.h"
#include "declaration.h"
#include "eval.h"
#include "expression.h"
#include "parse.h"
#include "symtab.h"
#include "typetree.h"
#include <lacc/array.h>
#include <lacc/context.h>
#include <lacc/token.h>

#include <assert.h>

#define set_break_target(old, brk) \
    old = break_target; \
    break_target = brk;

#define set_continue_target(old, cont) \
    old = continue_target; \
    continue_target = cont;

#define restore_break_target(old) \
    break_target = old;

#define restore_continue_target(old) \
    continue_target = old;

/*
 * Store reference to top of loop, for resolving break and continue. Use
 * call stack to keep track of depth, backtracking to the old value.
 */
static struct block
    *break_target,
    *continue_target;

struct switch_case {
    struct block *label;
    struct var value;
};

struct switch_context {
    struct block *default_label;
    array_of(struct switch_case) cases;
};

/*
 * Keep track of nested switch statements and their case labels. This
 * reference always points to the current context, and backtracking is
 * managed recursively by switch_statement.
 */
static struct switch_context *switch_context;

static void add_switch_case(struct block *label, struct var value)
{
    struct switch_case sc;

    sc.label = label;
    sc.value = value;
    array_push_back(&switch_context->cases, sc);
}

static void free_switch_context(struct switch_context *ctx)
{
    assert(ctx);
    array_clear(&ctx->cases);
    free(ctx);
}

static struct block *if_statement(
    struct preprocessor *input,
    struct definition *def,
    struct block *parent)
{
    struct block
        *right = cfg_block_init(def), *left,
        *next  = cfg_block_init(def);

    consume(input, IF);
    consume(input, '(');
    parent = expression(input, def, parent);
    if (!is_scalar(parent->expr.type)) {
        error(input, "If expression must have scalar type, was %t.",
            parent->expr.type);
        exit(1);
    }

    consume(input, ')');
    if (is_immediate_true(parent->expr)) {
        parent->jump[0] = right;
    } else if (is_immediate_false(parent->expr)) {
        parent->jump[0] = next;
    } else {
        parent->jump[0] = next;
        parent->jump[1] = right;
    }

    right = statement(input, def, right);
    right->jump[0] = next;
    if (peek(input).token == ELSE) {
        consume(input, ELSE);
        left = cfg_block_init(def);
        if (!is_immediate_true(parent->expr)) {
            /*
             * This block will be an orphan if the branch is immediate
             * taken true branch. Still need to evaluate the expression
             * here though.
             */
            parent->jump[0] = left;
        }
        left = statement(input, def, left);
        left->jump[0] = next;
    }

    return next;
}

static struct block *do_statement(
    struct preprocessor *input,
    struct definition *def,
    struct block *parent)
{
    struct block
        *top = cfg_block_init(def),
        *body,
        *cond = cfg_block_init(def),
        *tail,
        *next = cfg_block_init(def);

    struct block
        *old_break_target,
        *old_continue_target;

    set_break_target(old_break_target, next);
    set_continue_target(old_continue_target, cond);
    parent->jump[0] = top;

    consume(input, DO);
    body = statement(input, def, top);
    body->jump[0] = cond;
    consume(input, WHILE);
    consume(input, '(');
    tail = expression(input, def, cond);
    if (!is_scalar(tail->expr.type)) {
        error(input, "While expression must have scalar type, was %t.",
            tail->expr.type);
        exit(1);
    }

    consume(input, ')');
    if (is_immediate_true(tail->expr)) {
        tail->jump[0] = top;
    } else if (is_immediate_false(tail->expr)) {
        tail->jump[0] = next;
    } else {
        tail->jump[0] = next;
        tail->jump[1] = top;
    }

    restore_break_target(old_break_target);
    restore_continue_target(old_continue_target);
    return next;
}

static struct block *while_statement(
    struct preprocessor *input,
    struct definition *def,
    struct block *parent)
{
    struct block
        *top = cfg_block_init(def),
        *cond,
        *body = cfg_block_init(def),
        *next = cfg_block_init(def);

    struct block
        *old_break_target,
        *old_continue_target;

    set_break_target(old_break_target, next);
    set_continue_target(old_continue_target, top);
    parent->jump[0] = top;

    consume(input, WHILE);
    consume(input, '(');
    cond = expression(input, def, top);
    if (!is_scalar(cond->expr.type)) {
        error(input, "While expression must have scalar type, was %t.",
            cond->expr.type);
        exit(1);
    }

    consume(input, ')');
    if (is_immediate_true(cond->expr)) {
        cond->jump[0] = body;
    } else if (is_immediate_false(cond->expr)) {
        cond->jump[0] = next;
    } else {
        cond->jump[0] = next;
        cond->jump[1] = body;
    }

    body = statement(input, def, body);
    body->jump[0] = top;

    restore_break_target(old_break_target);
    restore_continue_target(old_continue_target);
    return next;
}

static struct block *for_statement(
    struct preprocessor *input,
    struct definition *def,
    struct block *parent)
{
    int declared;
    struct token tok;
    const struct symbol *sym;
    struct block
        *top = cfg_block_init(def),
        *body = cfg_block_init(def),
        *increment = cfg_block_init(def),
        *next = cfg_block_init(def);

    struct block
        *old_break_target,
        *old_continue_target;

    set_break_target(old_break_target, next);

    declared = 0;
    consume(input, FOR);
    consume(input, '(');
    switch ((tok = peek(input)).token) {
    case IDENTIFIER:
        sym = sym_lookup(&ns_ident, tok.d.string);
        if (!sym || sym->symtype != SYM_TYPEDEF) {
            parent = expression(input, def, parent);
            consume(input, ';');
            break;
        }
    case FIRST(type_name):
        declared = 1;
        push_scope(&ns_ident);
        parent = declaration(input, def, parent);
        break;
    default:
        parent = expression(input, def, parent);
    case ';':
        consume(input, ';');
        break;
    }

    if (peek(input).token != ';') {
        parent->jump[0] = top;
        top = expression(input, def, top);
        if (!is_scalar(top->expr.type)) {
            error(input,
                "Controlling expression must have scalar type, was %t.",
                top->expr.type);
            exit(1);
        }

        if (is_immediate_true(top->expr)) {
            top->jump[0] = body;
        } else if (is_immediate_false(top->expr)) {
            top->jump[0] = next;
        } else {
            top->jump[0] = next;
            top->jump[1] = body;
        }

        top = (struct block *) parent->jump[0];
    } else {
        /* Infinite loop. */
        parent->jump[0] = body;
        top = body;
    }

    consume(input, ';');
    if (peek(input).token != ')') {
        expression(input, def, increment)->jump[0] = top;
        consume(input, ')');
        set_continue_target(old_continue_target, increment);
        body = statement(input, def, body);
        body->jump[0] = increment;
    } else {
        consume(input, ')');
        set_continue_target(old_continue_target, top);
        body = statement(input, def, body);
        body->jump[0] = top;
    }

    restore_break_target(old_break_target);
    restore_continue_target(old_continue_target);
    if (declared) {
        pop_scope(&ns_ident);
    }

    return next;
}

static struct block *switch_statement(
    struct preprocessor *input,
    struct definition *def,
    struct block *parent)
{
    int i;
    struct var value;
    struct switch_case sc;
    struct block
        *cond,
        *prev_cond,
        *body = cfg_block_init(def),
        *last,
        *next = cfg_block_init(def);

    struct switch_context *old_switch_ctx;
    struct block *old_break_target;

    set_break_target(old_break_target, next);
    old_switch_ctx = switch_context;
    switch_context = calloc(1, sizeof(*switch_context));

    consume(input, SWITCH);
    consume(input, '(');
    parent = expression(input, def, parent);
    if (!is_integer(parent->expr.type)) {
        error(input, "Switch expression must have integer type, was %t.",
            parent->expr.type);
        exit(1);
    }
    consume(input, ')');
    last = statement(input, def, body);
    last->jump[0] = next;

    if (!array_len(&switch_context->cases) && !switch_context->default_label) {
        parent->jump[0] = next;
    } else {
        cond = parent;
        for (i = 0; i < array_len(&switch_context->cases); ++i) {
            prev_cond = cond;
            sc = array_get(&switch_context->cases, i);
            cond = cfg_block_init(def);
            value = eval(def, parent, parent->expr);
            cond->expr = eval_expr(input, def, cond, IR_OP_EQ, sc.value, value);
            cond->jump[1] = sc.label;
            prev_cond->jump[0] = cond;
        }

        cond->jump[0] = (switch_context->default_label) ?
            switch_context->default_label : next;
    }

    free_switch_context(switch_context);
    restore_break_target(old_break_target);
    switch_context = old_switch_ctx;
    return next;
}

INTERNAL struct block *statement(
    struct preprocessor *input,
    struct definition *def,
    struct block *parent)
{
    struct symbol *sym;
    struct token tok;

    switch ((tok = peek(input)).token) {
    case ';':
        consume(input, ';');
        break;
    case '{':
        parent = block(input, def, parent);
        break;
    case IF:
        parent = if_statement(input, def, parent);
        break;
    case DO:
        parent = do_statement(input, def, parent);
        consume(input, ';');
        break;
    case WHILE:
        parent = while_statement(input, def, parent);
        break;
    case FOR:
        parent = for_statement(input, def, parent);
        break;
    case GOTO:
        consume(input, GOTO);
        tok = consume(input, IDENTIFIER);
        sym = sym_add(
            input,
            &ns_label,
            tok.d.string,
            basic_type__void,
            SYM_TENTATIVE,
            LINK_INTERN);
        if (!sym->value.label) {
            sym->value.label = cfg_block_init(def);
        }
        parent->jump[0] = sym->value.label;
        parent = cfg_block_init(def); /* Orphan, unless labeled. */
        consume(input, ';');
        break;
    case CONTINUE:
    case BREAK:
        next(input);
        parent->jump[0] =
            (tok.token == CONTINUE) ? continue_target : break_target;
        consume(input, ';');
        parent = cfg_block_init(def); /* Orphan, unless labeled. */
        break;
    case RETURN:
        consume(input, RETURN);
        if (!is_void(type_next(def->symbol->type))) {
            parent = expression(input, def, parent);
            parent->expr = eval_return(input, def, parent);
        }
        consume(input, ';');
        parent = cfg_block_init(def); /* Orphan, unless labeled. */
        break;
    case SWITCH:
        parent = switch_statement(input, def, parent);
        break;
    case CASE:
        consume(input, CASE);
        if (!switch_context) {
            error(input,
                "Stray 'case' label, must be inside a switch statement.");
        } else {
            struct block *next = cfg_block_init(def);
            struct var expr = constant_expression(input);
            consume(input, ':');
            add_switch_case(next, expr);
            parent->jump[0] = next;
            next = statement(input, def, next);
            parent = next;
        }
        break;
    case DEFAULT:
        consume(input, DEFAULT);
        consume(input, ':');
        if (!switch_context) {
            error(input,
                "Stray 'default' label, must be inside a switch statement.");
        } else if (switch_context->default_label) {
            error(input, "Multiple 'default' labels inside the same switch.");
        } else {
            struct block *next = cfg_block_init(def);
            parent->jump[0] = next;
            switch_context->default_label = next;
            next = statement(input, def, next);
            parent = next;
        }
        break;
    case IDENTIFIER:
        if (peekn(input, 2).token == ':') {
            consume(input, IDENTIFIER);
            sym = sym_lookup(&ns_label, tok.d.string);
            if (sym && sym->symtype == SYM_DEFINITION) {
                error(input, "Duplicate label '%s'.", str_raw(tok.d.string));
            } else {
                sym = sym_add(
                    input,
                    &ns_label,
                    tok.d.string,
                    basic_type__void,
                    SYM_DEFINITION,
                    LINK_INTERN);
                if (!sym->value.label) {
                    assert(!sym->referenced);
                    sym->value.label = cfg_block_init(def);
                }
                parent->jump[0] = sym->value.label;
                parent = sym->value.label;
            }
            consume(input, ':');
            return statement(input, def, parent);
        }
        sym = sym_lookup(&ns_ident, tok.d.string);
        if (sym && sym->symtype == SYM_TYPEDEF) {
            parent = declaration(input, def, parent);
            break;
        }
        /* Fallthrough. */
    case NUMBER:
    case STRING:
    case '*':
    case '(':
    case INCREMENT:
    case DECREMENT:
        parent = expression(input, def, parent);
        parent->expr = eval_expression_statement(def, parent, parent->expr);
        consume(input, ';');
        break;
    default:
        parent = declaration(input, def, parent);
        break;
    }

    return parent;
}

/*
 * Treat statements and declarations equally, allowing declarations in
 * between statements as in modern C. Called compound-statement in K&R.
 */
INTERNAL struct block *block(
    struct preprocessor *input,
    struct definition *def,
    struct block *parent)
{
    consume(input, '{');
    push_scope(&ns_ident);
    push_scope(&ns_tag);
    while (peek(input).token != '}') {
        parent = statement(input, def, parent);
    }

    consume(input, '}');
    pop_scope(&ns_tag);
    pop_scope(&ns_ident);
    return parent;
}
