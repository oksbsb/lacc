#if !AMALGAMATION
# define INTERNAL
# define EXTERNAL extern
#endif
#include "directive.h"
#include "input.h"
#include "macro.h"
#include "preprocess.h"
#include "strtab.h"
#include "tokenize.h"
#include <lacc/array.h>
#include <lacc/context.h>

#include <assert.h>

#define IDENT(s) {IDENTIFIER, 0, 1, 0, {0}, {SHORT_STRING_INIT(s)}}

INTERNAL struct token
    ident__include = IDENT("include"),
    ident__defined = IDENT("defined"),
    ident__define = IDENT("define"),
    ident__ifndef = IDENT("ifndef"),
    ident__ifdef = IDENT("ifdef"),
    ident__undef = IDENT("undef"),
    ident__elif = IDENT("elif"),
    ident__endif = IDENT("endif"),
    ident__error = IDENT("error"),
    ident__line = IDENT("line"),
    ident__pragma = IDENT("pragma"),
    ident__Pragma = IDENT("_" "Pragma"),
    ident__VA_ARGS__ = IDENT("__VA_ARGS__");

struct number {
    Type type;
    union value val;
};

static void push_state(struct preprocessor *prep, enum state c)
{
    array_push_back(&prep->branch_stack, c);
}

static enum state pop_state(struct preprocessor *prep)
{
    enum state val;

    if (!array_len(&prep->branch_stack)) {
        error(prep, "Unmatched #endif directive.");
        exit(1);
    }

    val = array_pop_back(&prep->branch_stack);
    if (!array_len(&prep->branch_stack)) {
        array_clear(&prep->branch_stack);
    }

    return val;
}

INTERNAL int in_active_block(struct preprocessor *prep)
{
    enum state s = BRANCH_LIVE;
    if (array_len(&prep->branch_stack)) {
        s = array_get(&prep->branch_stack, array_len(&prep->branch_stack) - 1);
    }

    return s == BRANCH_LIVE;
}

static void expect(
    struct preprocessor *prep,
    const struct token *list,
    int token)
{
    String a, b;
    if (list->token != token) {
        a = basic_token[token].d.string;
        b = list->d.string;
        error(prep, "Expected '%s', but got '%s'.", str_raw(a), str_raw(b));
        exit(1);
    }
}

static struct number preprocess_expression(
    struct preprocessor *prep,
    const struct token *list,
    const struct token **endptr);

/*
 * Macro expansions should already have been done. Stray identifiers are
 * interpreted as zero constants.
 *
 * Normalize all values to be of type long or unsigned long. Floating
 * point numbers are not permitted by the standard.
 */
static struct number preprocess_primary(
    struct preprocessor *prep,
    const struct token *list,
    const struct token **endptr)
{
    String s;
    struct token n;
    struct number num;

    switch (list->token) {
    case PREP_NUMBER:
        n = convert_preprocessing_number(prep, *list);
        assert(n.token == NUMBER);
        num.type = n.type;
        num.val = n.d.val;
        break;
    case PREP_CHAR:
        n = convert_preprocessing_char(prep, *list);
        assert(n.token == NUMBER);
        num.type = n.type;
        num.val = n.d.val;
        break;
    case '(':
        num = preprocess_expression(prep, list + 1, &list);
        expect(prep, list, ')');
        break;
    default:
        if (!list->is_expandable) {
            s = list->d.string;
            error(prep,
                "Invalid primary expression '%s' in directive.", str_raw(s));
            exit(1);
        }
    case IDENTIFIER:
        num.type = basic_type__long;
        num.val.i = 0;
        break;
    }

    *endptr = list + 1;
    if (!is_integer(num.type)) {
        error(prep, "Preprocessing number must be integer.");
        exit(1);
    } else if (size_of(num.type) != 8) {
        if (is_signed(num.type)) {
            num.val = convert(num.val, num.type, basic_type__long);
            num.type = basic_type__long;
        } else {
            num.val = convert(num.val, num.type, basic_type__unsigned_long);
            num.type = basic_type__unsigned_long;
        }
    }

    return num;
}

static struct number preprocess_unary(
    struct preprocessor *prep,
    const struct token *list,
    const struct token **endptr)
{
    struct number r;

    switch (list->token) {
    case '+':
        r = preprocess_unary(prep, list + 1, endptr);
        if (is_signed(r.type)) {
            r.val.i = + r.val.i;
        }
        return r;
    case '-':
        r = preprocess_unary(prep, list + 1, endptr);
        if (is_signed(r.type)) {
            r.val.i = - r.val.i;
        } else {
            r.val.u = - r.val.u;
        }
        return r;
    case '~':
        r = preprocess_unary(prep, list + 1, endptr);
        if (is_signed(r.type)) {
            r.val.i = ~ r.val.i;
        } else {
            r.val.u = ~ r.val.u;
        }
        return r;
    case '!':
        r = preprocess_unary(prep, list + 1, endptr);
        if (is_signed(r.type)) {
            r.val.i = ! r.val.i;
        } else {
            r.val.u = ! r.val.u;
        }
        return r;
    default:
        return preprocess_primary(prep, list, endptr);
    }
}

static int both_signed(struct number *l, struct number *r)
{
    Type type;

    if (!type_equal(l->type, r->type)) {
        type = usual_arithmetic_conversion(l->type, r->type);
        l->val = convert(l->val, l->type, type);
        l->type = type;
        r->val = convert(r->val, r->type, type);
        r->type = type;
    } else {
        type = l->type;
    }

    assert(is_integer(type));
    return is_signed(type);
}

static struct number preprocess_multiplicative(
    struct preprocessor *prep,
    const struct token *list,
    const struct token **endptr)
{
    struct number l, r;

    l = preprocess_unary(prep, list, &list);
    while (1) {
        switch (list->token) {
        case '*':
            r = preprocess_unary(prep, list + 1, &list);
            if (both_signed(&l, &r)) {
                l.val.i *= r.val.i;
            } else {
                l.val.u *= r.val.u;
            }
            break;
        case '/':
            r = preprocess_unary(prep, list + 1, &list);
            if (r.val.i == 0) {
                error(prep, "Division by zero.");
                exit(1);
            } else if (both_signed(&l, &r)) {
                l.val.i /= r.val.i;
            } else {
                l.val.u /= r.val.u;
            }
            break;
        case '%':
            r = preprocess_unary(prep, list + 1, &list);
            if (r.val.i == 0) {
                error(prep, "Modulo by zero.");
                exit(1);
            } else if (both_signed(&l, &r)) {
                l.val.i %= r.val.i;
            } else {
                l.val.u %= r.val.u;
            }
            break;
        default:
            *endptr = list;
            return l;
        }
    }
}

static struct number preprocess_additive(
    struct preprocessor *prep,
    const struct token *list,
    const struct token **endptr)
{
    struct number l, r;

    l = preprocess_multiplicative(prep, list, &list);
    while (1) {
        switch (list->token) {
        case '+':
            r = preprocess_multiplicative(prep, list + 1, &list);
            if (both_signed(&l, &r)) {
                l.val.i += r.val.i;
            } else {
                l.val.u += r.val.u;
            }
            break;
        case '-':
            r = preprocess_multiplicative(prep, list + 1, &list);
            if (both_signed(&l, &r)) {
                l.val.i -= r.val.i;
            } else {
                l.val.u -= r.val.u;
            }
            break;
        default:
            *endptr = list;
            return l;
        }
    }
}

static struct number preprocess_shift(
    struct preprocessor *prep,
    const struct token *list,
    const struct token **endptr)
{
    struct number l, r;

    l = preprocess_additive(prep, list, &list);
    while (1) {
        switch (list->token) {
        case LSHIFT:
            r = preprocess_additive(prep, list + 1, &list);
            if (both_signed(&l, &r)) {
                l.val.i <<= r.val.i;
            } else {
                l.val.u <<= r.val.u;
            }
            break;
        case RSHIFT:
            r = preprocess_additive(prep, list + 1, &list);
            if (both_signed(&l, &r)) {
                l.val.i >>= r.val.i;
            } else {
                l.val.u >>= r.val.u;
            }
            break;
        default:
            *endptr = list;
            return l;
        }
    }
}

static struct number preprocess_relational(
    struct preprocessor *prep,
    const struct token *list,
    const struct token **endptr)
{
    struct number l, r;

    l = preprocess_shift(prep, list, &list);
    while (1) {
        switch (list->token) {
        case '<':
            r = preprocess_shift(prep, list + 1, &list);
            l.val.i = both_signed(&l, &r)
                    ? l.val.i < r.val.i
                    : l.val.u < r.val.u;
            l.type = basic_type__long;
            break;
        case '>':
            r = preprocess_shift(prep, list + 1, &list);
            l.val.i = both_signed(&l, &r)
                    ? l.val.i > r.val.i
                    : l.val.u > r.val.u;
            l.type = basic_type__long;
            break;
        case LEQ:
            r = preprocess_shift(prep, list + 1, &list);
            l.val.i = both_signed(&l, &r)
                    ? l.val.i <= r.val.i
                    : l.val.u <= r.val.u;
            l.type = basic_type__long;
            break;
        case GEQ:
            r = preprocess_shift(prep, list + 1, &list);
            l.val.i = both_signed(&l, &r)
                    ? l.val.i >= r.val.i
                    : l.val.u >= r.val.u;
            l.type = basic_type__long;
            break;
        default:
            *endptr = list;
            return l;
        }
    }
}

static struct number preprocess_equality(
    struct preprocessor *prep,
    const struct token *list,
    const struct token **endptr)
{
    struct number l, r;

    l = preprocess_relational(prep, list, &list);
    switch (list->token) {
    case EQ:
        r = preprocess_equality(prep, list + 1, &list);
        l.val.i = both_signed(&l, &r)
                ? l.val.i == r.val.i
                : l.val.u == r.val.u;
        l.type = basic_type__long;
        break;
    case NEQ:
        r = preprocess_equality(prep, list + 1, &list);
        l.val.i = both_signed(&l, &r)
                ? l.val.i != r.val.i
                : l.val.u != r.val.u;
        l.type = basic_type__long;
        break;
    default:
        break;
    }

    *endptr = list;
    return l;
}

static struct number preprocess_and(
    struct preprocessor *prep,
    const struct token *list,
    const struct token **endptr)
{
    struct number l, r;

    l = preprocess_equality(prep, list, &list);
    if (list->token == '&') {
        r = preprocess_and(prep, list + 1, &list);
        if (both_signed(&l, &r)) {
            l.val.i &= r.val.i;
        } else {
            l.val.u &= r.val.u;
        }
    }

    *endptr = list;
    return l;
}

static struct number preprocess_exclusive_or(
    struct preprocessor *prep,
    const struct token *list,
    const struct token **endptr)
{
    struct number l, r;

    l = preprocess_and(prep, list, &list);
    if (list->token == '^') {
        r = preprocess_exclusive_or(prep, list + 1, &list);
        if (both_signed(&l, &r)) {
            l.val.i ^= r.val.i;
        } else {
            l.val.u ^= r.val.u;
        }
    }

    *endptr = list;
    return l;
}

static struct number preprocess_inclusive_or(
    struct preprocessor *prep,
    const struct token *list,
    const struct token **endptr)
{
    struct number l, r;

    l = preprocess_exclusive_or(prep, list, &list);
    if (list->token == '|') {
        r = preprocess_exclusive_or(prep, list + 1, &list);
        if (both_signed(&l, &r)) {
            l.val.i |= r.val.i;
        } else {
            l.val.u |= r.val.u;
        }
    }

    *endptr = list;
    return l;
}

static struct number preprocess_logical_and(
    struct preprocessor *prep,
    const struct token *list,
    const struct token **endptr)
{
    struct number l, r;

    l = preprocess_inclusive_or(prep, list, &list);
    if (list->token == LOGICAL_AND) {
        r = preprocess_logical_and(prep, list + 1, &list);
        l.val.i = l.val.u && r.val.u;
        l.type = basic_type__long;
    }

    *endptr = list;
    return l;
}

static struct number preprocess_logical_or(
    struct preprocessor *prep,
    const struct token *list,
    const struct token **endptr)
{
    struct number l, r;

    l = preprocess_logical_and(prep, list, &list);
    if (list->token == LOGICAL_OR) {
        r = preprocess_logical_or(prep, list + 1, &list);
        l.val.i = l.val.u || r.val.u;
        l.type = basic_type__long;
    }

    *endptr = list;
    return l;
}

static struct number preprocess_conditional(
    struct preprocessor *prep,
    const struct token *list,
    const struct token **endptr)
{
    struct number a, b, c;

    a = preprocess_logical_or(prep, list, &list);
    if (list->token == '?') {
        b = preprocess_expression(prep, list + 1, &list);
        expect(prep, list, ':');
        c = preprocess_conditional(prep, list + 1, &list);
        if (both_signed(&b, &c)) {
            a.val.i = a.val.u ? b.val.i : c.val.i;
            a.type = basic_type__long;
        } else {
            a.val.u = a.val.u ? b.val.u : c.val.u;
            a.type = basic_type__unsigned_long;
        }
    }

    *endptr = list;
    return a;
}

static struct number preprocess_expression(
    struct preprocessor *prep,
    const struct token *list,
    const struct token **endptr)
{
    struct number n;

    do {
        n = preprocess_conditional(prep, list, &list);
    } while (list->token == ',' && list++);

    *endptr = list;
    return n;
}

static struct number preprocess_constant_expression(
    struct preprocessor *prep,
    const struct token *list,
    const struct token **endptr)
{
    return preprocess_conditional(prep, list, endptr);
}

/*
 * Preprocess include directive, which should have any of the following
 * forms:
 *
 *     #include "foo.h"
 *     #include <foo.h>
 *     #include FOO
 *
 * Macro expansion is performed if neither of the first two forms match.
 */
static void preprocess_include(struct preprocessor *prep, TokenArray *line)
{
    int i, len, exp, found;
    String path;
    const char *name;
    struct token t;

    assert(!tok_cmp(array_get(line, 0), ident__include));
    assert(array_back(line).token == NEWLINE);

    name = NULL;
    found = 0;
    array_erase(line, 0);
    for (exp = 0; exp < 2; ++exp) {
        len = array_len(line);
        t = array_get(line, 0);
        if (t.token == PREP_STRING) {
            if (len > 2) {
                error(prep, "Stray tokens in include directive.");
                exit(1);
            }

            name = str_raw(t.d.string);
            found = include_file(prep->input, &prep->strtab, name);
            break;
        }

        if (t.token == '<' && array_get(line, len - 2).token == '>') {
            path = str_init("");
            for (i = 1; i < len - 2; ++i) {
                t = array_get(line, i);
                path = str_cat(&prep->strtab, path, t.d.string);
            }

            name = str_raw(path);
            found = include_system_file(prep->input, &prep->strtab, name);
            break;
        }

        if (!exp) {
            expand(prep, line);
        } else {
            error(prep, "Invalid include directive.");
            exit(1);
        }
    }

    if (!found) {
        error(prep, "Unable to resolve include file '%s'.", name);
        exit(1);
    }
}

/* Function-like macro iff parenthesis immediately after identifier. */
static struct macro preprocess_define(
    struct preprocessor *prep,
    const struct token *line,
    const struct token **endptr)
{
    struct macro macro = {0};
    struct token param = {PARAM}, t;
    TokenArray params = get_token_array(prep);
    int i;

    t = *line++;
    if (!t.is_expandable) {
        error(prep, "Invalid definition of %s as a macro", str_raw(t.d.string));
        exit(1);
    }

    macro.name = t.d.string;
    macro.type = OBJECT_LIKE;
    macro.replacement = get_token_array(prep);
    if (line->token == '(' && !line->leading_whitespace) {
        macro.type = FUNCTION_LIKE;
        line++;
        while (line->token != ')') {
            if (line->token == DOTS) {
                macro.is_vararg = 1;
                array_push_back(&params, ident__VA_ARGS__);
                line++;
                break;
            }
            if (line->token != IDENTIFIER) {
                error(prep, "Invalid macro parameter, expected identifer.");
                exit(1);
            }
            array_push_back(&params, *line++);
            if (line->token != ',')
                break;
            line++;
        }
        expect(prep, line, ')');
        line++;
    }

    macro.params = array_len(&params);

    while (line->token != NEWLINE) {
        assert(line->token != END);
        param.d.val.i = -1;
        if (line->token == IDENTIFIER && macro.type == FUNCTION_LIKE) {
            for (i = 0; i < macro.params; ++i) {
                if (!tok_cmp(*line, array_get(&params, i))) {
                    param.d.val.i = i;
                    break;
                }
            }
        }
        if (param.d.val.i != -1) {
            array_push_back(&macro.replacement, param);
        } else {
            array_push_back(&macro.replacement, *line);
        }
        line++;
    }

    *endptr = line;
    release_token_array(prep, params);
    return macro;
}

/*
 * Handle #line directive, which is in one of the following forms:
 *
 *     #line 42
 *     #line 42 "foo.c"
 *
 * Update line number, and optionally name, of file being processed.
 */
static void preprocess_line_directive(
    struct preprocessor *prep,
    const struct token *line)
{
    struct token t;

    t = *line++;
    if (t.token == PREP_NUMBER) {
        t = convert_preprocessing_number(prep, t);
    }

    if (t.token != NUMBER || !is_int(t.type) || t.d.val.i <= 0) {
        error(prep, "Expected positive integer in #line directive.");
        exit(1);
    }

    input_set_line(prep->input, t.d.val.i - 1);
    if (line->token == PREP_STRING) {
        input_set_file(prep->input, line->d.string);
        line++;
    }

    if (line->token != NEWLINE) {
        error(prep, "Unexpected token in #line directive.");
        exit(1);
    }
}

INTERNAL void preprocess_directive(
    struct preprocessor *prep,
    TokenArray *array)
{
    struct number num;
    int def;
    enum state state;
    String s;
    const struct token *line = array->data;

    /*
     * Perform macro expansion only for if, elif and line directives,
     * before doing any expression parsing.
     */
    if (line->token == IF
        || !tok_cmp(*line, ident__elif)
        || (in_active_block(prep) && !tok_cmp(*line, ident__line)))
    {
        expand(prep, array);
        line = array->data;
    }

    if (line->token == IF) {
        /*
         * Expressions are not necessarily valid in dead blocks, for
         * example can function-like macros be undefined.
         */
        if (in_active_block(prep)) {
            num = preprocess_constant_expression(prep, line + 1, &line);
            push_state(prep, num.val.i ? BRANCH_LIVE : BRANCH_DEAD);
        } else {
            push_state(prep, BRANCH_DISABLED);
        }
    } else if (line->token == ELSE) {
        state = pop_state(prep);
        if (in_active_block(prep)) {
            push_state(prep,
                state == BRANCH_DEAD ? BRANCH_LIVE : BRANCH_DISABLED);
        } else {
            assert(state == BRANCH_DISABLED);
            push_state(prep, BRANCH_DISABLED);
        }
    } else if (!tok_cmp(*line, ident__elif)) {
        state = pop_state(prep);
        if (in_active_block(prep)) {
            if (state == BRANCH_DEAD) {
                num = preprocess_constant_expression(prep, line + 1, &line);
                push_state(prep, num.val.i ? BRANCH_LIVE : BRANCH_DEAD);
            } else {
                push_state(prep, BRANCH_DISABLED);
            }
        } else {
            assert(state == BRANCH_DISABLED);
            push_state(prep, BRANCH_DISABLED);
        }
    } else if (!tok_cmp(*line, ident__endif)) {
        pop_state(prep);
    } else if (!tok_cmp(*line, ident__ifndef)) {
        if (in_active_block(prep)) {
            line++;
            if (!line->is_expandable) {
                error(prep, "Expected identifier in 'ifndef' clause.");
                exit(1);
            }
            def = macro_definition(prep, line->d.string) == NULL;
            push_state(prep, def ? BRANCH_LIVE : BRANCH_DEAD);
        } else {
            push_state(prep, BRANCH_DISABLED);
        }
    } else if (!tok_cmp(*line, ident__ifdef)) {
        if (in_active_block(prep)) {
            line++;
            if (!line->is_expandable) {
                error(prep, "Expected identifier in 'ifdef' clause.");
                exit(1);
            }
            def = macro_definition(prep, line->d.string) != NULL;
            push_state(prep, def ? BRANCH_LIVE : BRANCH_DEAD);
        } else {
            push_state(prep, BRANCH_DISABLED);
        }
    } else if (in_active_block(prep)) {
        if (!tok_cmp(*line, ident__define)) {
            define(prep, preprocess_define(prep, line + 1, &line));
        } else if (!tok_cmp(*line, ident__undef)) {
            line++;
            if (!line->is_expandable) {
                error(prep, "Expected identifier in 'undef' clause.");
                exit(1);
            }
            undef(prep, line->d.string);
        } else if (!tok_cmp(*line, ident__include)) {
            preprocess_include(prep, array);
        } else if (!tok_cmp(*line, ident__line)) {
            preprocess_line_directive(prep, line + 1);
        } else if (!tok_cmp(*line, ident__error)) {
            array->data++;
            array->length--;
            s = stringify(prep, array).d.string;
            error(prep, "%s", str_raw(s));
            exit(1);
        } else {
            s = line->d.string;
            error(prep, "Unsupported preprocessor directive '%s'.", str_raw(s));
            exit(1);
        }
    }
}
