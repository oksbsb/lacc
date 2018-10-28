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
#include <lacc/context.h>

#include <assert.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>

static int is_directive(const char *line)
{
    while (*line == ' ' || *line == '\t') {
        line++;
    }

    return *line == '#';
}

static struct token get_token(struct preprocessor *prep)
{
    struct token t;

    if (!prep->line) {
        do {
            prep->line = getprepline(prep->input);
            if (!prep->line) {
                return basic_token[END];
            }

        } while (!in_active_block(prep) && !is_directive(prep->line));
    }

    t = tokenize(prep, prep->line, &prep->line);
    if (t.token == END) {
        /*
         * Newlines are removed by getprepline, and never present in
         * the input data. Instead intercept end of string, which
         * represents end of line.
         */
        t = basic_token[NEWLINE];
        prep->line = NULL;
    }

    return t;
}

INTERNAL void preprocess_init(struct preprocessor *prep, struct input *input)
{
    prep->input = input;
    prep->line = NULL;

    macro_init(&prep->macros);
    strtab_init(&prep->strtab);
    deque_empty(&prep->lookahead);
    register_builtin_definitions(prep, context.standard);
}

INTERNAL void preprocess_finalize(struct preprocessor *prep)
{
    int i;
    TokenArray list;
    ExpandStack stack;

    strtab_destroy(&prep->strtab);
    deque_destroy(&prep->lookahead);
    hash_destroy(&prep->macros);

    for (i = 0; i < array_len(&prep->arrays); ++i) {
        list = array_get(&prep->arrays, i);
        array_clear(&list);
    }

    for (i = 0; i < array_len(&prep->stacks); ++i) {
        stack = array_get(&prep->stacks, i);
        array_clear(&stack);
    }

    array_clear(&prep->arrays);
    array_clear(&prep->stacks);
}

static enum token_type read_through_newline(
    struct preprocessor *prep,
    TokenArray *line)
{
    struct token t;

    t = get_token(prep);
    while (t.token == NEWLINE) {
        t = get_token(prep);
    }

    if (t.token != END) {
        array_push_back(line, t);
    }

    return t.token;
}

/*
 * _Pragma invocations differ slighly from macro expansions, in that the
 * opening parenthesis can start on a newline.
 */
static void read_Pragma_invocation(struct preprocessor *prep, TokenArray *line)
{
    enum token_type t;

    t = read_through_newline(prep, line);
    if (t != '(') {
        error(prep, "Expected '(' after _Pragma.");
        exit(1);
    }

    t = read_through_newline(prep, line);
    if (t != STRING && t != PREP_STRING) {
        error(prep, "Invalid argument to _Pragma operator, expected string.");
        exit(1);
    }

    t = read_through_newline(prep, line);
    if (t != ')') {
        error(prep, "Expected ')' to complete _Pragma expression.");
        exit(1);
    }
}

/*
 * Keep track of the nesting depth of macro arguments. For example;
 * MAX( MAX(10, 12), 20 ) should complete on the last parenthesis, which
 * makes the expression balanced. Read lines until full macro invocation
 * is included.
 */
static void read_macro_invocation(
    struct preprocessor *prep,
    TokenArray *line,
    const struct macro *macro)
{
    int nesting;
    struct token t;
    assert(macro->type == FUNCTION_LIKE);

    t = get_token(prep);
    array_push_back(line, t);
    if (t.token != '(')
        /*
         * Only expand function-like macros if they appear as function
         * invocations, beginning with an open paranthesis.
         */
        return;

    nesting = 1;
    while (nesting) {
        t = get_token(prep);
        if (t.token == '(') {
            nesting++;
        }
        if (t.token == ')') {
            assert(nesting);
            nesting--;
        }
        if (t.token == NEWLINE) {
            /*
             * This is the only scenario where reading a line is not
             * enough. Macro invocations can span lines, and we want to
             * have everything in the same token list.
             */
            continue;
        }
        assert(t.token != END);
        array_push_back(line, t);
    }

    if (nesting) {
        error(prep,
            "Unbalanced invocation of macro '%s'.",
            str_raw(macro->name));
        exit(1);
    }
}

/* Replace 'defined name' and 'defined (name)' with 0 or 1 constants. */
static void read_defined_operator(struct preprocessor *prep, TokenArray *line)
{
    int is_parens = 0;
    const char *endptr;
    struct token t = get_token(prep);

    if (t.token == '(') {
        t = get_token(prep);
        is_parens = 1;
    }

    if (!t.is_expandable) {
        error(prep, "Expected identifier in 'defined' clause, but got '%s'",
            str_raw(t.d.string));
        exit(1);
    }

    if (macro_definition(prep, t.d.string))
        t = tokenize(prep, "1", &endptr);
    else
        t = tokenize(prep, "0", &endptr);

    array_push_back(line, t);
    if (is_parens) {
        t = get_token(prep);
        if (t.token != ')') {
            error(prep, "Expected ')' to close 'defined' clause.");
            exit(1);
        }
    }
}

/*
 * Get token at position i of existing line, or add new token from input
 * stream to line at posistion. Overwrite the trailing newline.
 */
static struct token skip_or_get_token(
    struct preprocessor *prep,
    TokenArray *line,
    int i)
{
    struct token t;

    if (i == array_len(line) - 1) {
        t = array_get(line, i);
        if (t.token == NEWLINE) {
            (void) array_pop_back(line);
        }
    }

    if (i == array_len(line)) {
        do {
            t = get_token(prep);
        } while (t.token == NEWLINE);
        assert(t.token != END);
        array_push_back(line, t);
    } else {
        assert(i >= 0);
        assert(i < array_len(line));
        t = array_get(line, i);
    }

    return t;
}

/*
 * Make sure expanded token list contains enough tokens to do additional
 * expansions. Read more input if the provided function-like macro at
 * posistion i does not have all parameters on the current line.
 */
static int skip_or_read_expansion(
    struct preprocessor *prep,
    TokenArray *line,
    int i)
{
    int start = i, nest;
    struct token t;

    t = skip_or_get_token(prep, line, i++);
    if (t.token != '(') {
        return i - start;
    }

    nest = 1;
    while (nest) {
        t = skip_or_get_token(prep, line, i++);
        if (t.token == '(') nest++;
        if (t.token == ')') nest--;
    }

    return i - start;
}

/*
 * Read tokens until reaching end of line. If initial token is '#', stop
 * on first newline. Otherwise make sure _Pragma and macro invocations
 * spanning multiple lines are joined. Replace 'defined' with 0 or 1.
 *
 * Returns a buffer containing all necessary tokens to preprocess a
 * line. Always ends with a newline (\n) token, but never contains any
 * newlines in the array itself.
 */
static int read_complete_line(
    struct preprocessor *prep,
    TokenArray *line,
    struct token t,
    int directive)
{
    int expandable = 1, macros = 0;
    const struct macro *def;

    if (directive) {
        array_push_back(line, t);
        expandable = (t.token == IF) || !tok_cmp(t, ident__elif);
        t = get_token(prep);
    }

    while (t.token != NEWLINE) {
        assert(t.token != END);
        if (expandable && t.is_expandable) {
            if (directive && !tok_cmp(t, ident__defined)) {
                read_defined_operator(prep, line);
            } else if (!tok_cmp(t, ident__Pragma)) {
                array_push_back(line, t);
                read_Pragma_invocation(prep, line);
            } else {
                def = macro_definition(prep, t.d.string);
                if (def) {
                    macros += 1;
                    if (def->type == FUNCTION_LIKE) {
                        array_push_back(line, t);
                        read_macro_invocation(prep, line, def);
                    } else {
                        array_push_back(line, t);
                    }
                } else {
                    array_push_back(line, t);
                }
            }
        } else {
            array_push_back(line, t);
        }
        t = get_token(prep);
    }

    assert(t.token == NEWLINE);
    array_push_back(line, t);
    return macros;
}

/*
 * After expansion, it might be that we need to read a bit more input to
 * get argument of new expansion. Look through the array and see whether
 * there is a partial macro invocation that needs more input.
 *
 * Also handle _Pragma directives, which can span multiple lines.
 *
 * Return non-zero if there are more function-like macros that needs to
 * be expanded.
 */
static int refill_expanding_line(struct preprocessor *prep, TokenArray *line)
{
    int i, n, len;
    struct token t;
    const struct macro *def;

    len = array_len(line);
    if (!len) {
        return 0;
    }

    for (i = 0, n = 0; i < len; ++i) {
        t = array_get(line, i);
        if (t.is_expandable && !t.disable_expand) {
            def = macro_definition(prep, t.d.string);
            if (def && def->type == FUNCTION_LIKE) {
                i += skip_or_read_expansion(prep, line, i + 1);
                n += 1;
            }
        } else if (!tok_cmp(t, ident__Pragma)) {
            i += skip_or_read_expansion(prep, line, i + 1);
        }
    }

    /* Make sure a complete line is read, to not mix directives. */
    if (t.token != NEWLINE) {
        t = get_token(prep);
        n += read_complete_line(prep, line, t, 0);
    }

    return n;
}

static const char *stringify_token(const struct token *t)
{
    static char buf[512];

    switch (t->token) {
    case PARAM:
        assert(0);
    case NUMBER:
        if (is_unsigned(t->type)) {
            if (size_of(t->type) == 8) {
                sprintf(buf, "%luul", t->d.val.u);
            } else {
                sprintf(buf, "%luu", t->d.val.u);
            }
        } else if (is_signed(t->type)) {
            if (size_of(t->type) == 8) {
                sprintf(buf, "%ldl", t->d.val.i);
            } else {
                sprintf(buf, "%ld", t->d.val.i);
            }
        } else if (is_float(t->type)) {
            sprintf(buf, "%ff", t->d.val.f);
        } else if (is_double(t->type)) {
            sprintf(buf, "%f", t->d.val.d);
        } else {
            assert(is_long_double(t->type));
            sprintf(buf, "%Lf", t->d.val.ld);
        }
        return buf;
    default:
        return str_raw(t->d.string);
    }
}

/*
 * Add preprocessed token to lookahead buffer, ready to be consumed by
 * the parser.
 *
 * This is the last step of preprocessing, where we also do join of
 * adjacent string literals, and conversion from preprocessing number to
 * proper numeric values.
 */
static void add_to_lookahead(struct preprocessor *prep, struct token t)
{
    struct token prev;

    if (context.target != TARGET_PREPROCESS) {
        switch (t.token) {
        case PREP_CHAR:
            t = convert_preprocessing_char(prep, t);
            break;
        case PREP_NUMBER:
            t = convert_preprocessing_number(prep, t);
            break;
        case PREP_STRING:
            t = convert_preprocessing_string(prep, t);
        case STRING:
            if (deque_len(&prep->lookahead)) {
                prev = deque_back(&prep->lookahead);
                if (prev.token == STRING) {
                    t.d.string = str_cat(
                        &prep->strtab,
                        prev.d.string,
                        t.d.string);
                    deque_back(&prep->lookahead) = t;
                    goto added;
                }
            }
        default:
            break;
        }
    }

    deque_push_back(&prep->lookahead, t);

added:
    if (context.verbose) {
        verbose("   token( %s )", stringify_token(&t));
    }
}

/*
 * Determine whether we need to read more input in anticipation of a new
 * string literal needing to be joined with the current lookahead. This
 * is the case if buffer is non-empty, and last element is STRING, which
 * can be followed by any number of NEWLINE.
 */
static int is_lookahead_ready(struct preprocessor *prep, int n)
{
    unsigned len;
    struct token last;

    len = deque_len(&prep->lookahead);
    if (len < n) {
        return 0;
    }

    if (len > 0 && context.target != TARGET_PREPROCESS) {
        last = deque_back(&prep->lookahead);
        if (last.token == STRING) {
            return 0;
        }
    }

    return 1;
}

static void preprocess_pragma(struct preprocessor *prep, TokenArray *line)
{
    int i;
    struct token t;

    assert(array_len(line) > 0);
    assert(!tok_cmp(ident__pragma, array_get(line, 0)));
    if (context.target == TARGET_PREPROCESS) {
        add_to_lookahead(prep, basic_token[NEWLINE]);
        add_to_lookahead(prep, basic_token['#']);
        for (i = 0; i < array_len(line); ++i) {
            t = array_get(line, i);
            assert(t.token != END);
            add_to_lookahead(prep, t);
        }
        if (t.token != NEWLINE) {
            add_to_lookahead(prep, basic_token[NEWLINE]);
        }
    } else {
        /* Pragma directives are not yet handled. */
    }
}

/*
 * Replace \" by ", and \\ by \, returning a new string that can be
 * tokenized.
 */
static const char *destringize(String str, struct strtab *strtab)
{
    int i;
    char *ptr;
    size_t len;
    const char *raw;

    len = str.len;
    raw = str_raw(str);
    if (len + 1 > strtab->catlen) {
        strtab->catlen = len + 1;
        strtab->catbuf = realloc(strtab->catbuf, strtab->catlen);
        memset(strtab->catbuf, '\0', strtab->catlen);
    }

    ptr = strtab->catbuf;
    for (i = 0; i < len; ++i) {
        if (raw[i] == '\\') {
            switch (raw[i + 1]) {
            default:
                *ptr++ = raw[i];
            case '\"':
            case '\\':
                i++;
                *ptr++ = raw[i];
                break;
            }
        } else {
            *ptr++ = raw[i];
        }
    }

    return strtab->catbuf;
}

static int preprocess_Pragma(
    struct preprocessor *prep,
    TokenArray *line,
    int i,
    TokenArray *pragma)
{
    struct token t;
    const char *destr, *endptr;

    array_empty(pragma);
    t = array_get(line, i);
    assert(!tok_cmp(t, ident__Pragma));
    if (array_len(line) - i < 3
        || array_get(line, i + 1).token != '('
        || (array_get(line, i + 2).token != STRING
            && array_get(line, i + 2).token != PREP_STRING)
        || array_get(line, i + 3).token != ')')
    {
        error(prep, "Wrong application of _Pragma operator.");
        exit(1);
    }

    array_push_back(pragma, ident__pragma);

    t = array_get(line, i + 2);
    destr = destringize(t.d.string, &prep->strtab);
    while ((t = tokenize(prep, destr, &endptr)).token != END) {
        array_push_back(pragma, t);
        destr = endptr;
    }

    array_get(pragma, 1).leading_whitespace = 1;
    return i + 3;
}

/*
 * Consume at least one line, up until the final newline or end of file.
 * Fill up lookahead buffer to hold at least n tokens. In case of end of
 * input, put END tokens in remaining slots.
 */
static void preprocess_line(struct preprocessor *prep, int n)
{
    static TokenArray line, pragma;

    int i;
    struct token t;
    assert(prep);

    do {
        t = get_token(prep);
        if (t.token == END) {
            array_clear(&line);
            array_clear(&pragma);
            break;
        }

        line.length = 0;
        if (t.token == '#') {
            t = get_token(prep);
            if ((t.token != NEWLINE && in_active_block(prep))
                || t.token == IF
                || t.token == ELSE
                || !tok_cmp(t, ident__ifdef)
                || !tok_cmp(t, ident__ifndef)
                || !tok_cmp(t, ident__elif)
                || !tok_cmp(t, ident__endif))
            {
                read_complete_line(prep, &line, t, 1);
                if (!tok_cmp(t, ident__pragma)) {
                    preprocess_pragma(prep, &line);
                } else {
                    preprocess_directive(prep, &line);
                }
            } else {
                prep->line = NULL;
            }
        } else {
            assert(in_active_block(prep));
            i = read_complete_line(prep, &line, t, 0);
            while (i && expand(prep, &line)) {
                i = refill_expanding_line(prep, &line);
            }
            for (i = 0; i < array_len(&line); ++i) {
                t = array_get(&line, i);
                if (!tok_cmp(t, ident__Pragma)) {
                    i = preprocess_Pragma(prep, &line, i, &pragma);
                    preprocess_pragma(prep, &pragma);
                } else if (t.token != NEWLINE
                    || context.target == TARGET_PREPROCESS)
                {
                    add_to_lookahead(prep, t);
                }
            }
        }
    } while (!is_lookahead_ready(prep, n));

    while (deque_len(&prep->lookahead) < n) {
        add_to_lookahead(prep, basic_token[END]);
    }
}

INTERNAL void inject_line(struct preprocessor *prep, const char *line)
{
    assert(!prep->line);

    prep->line = line;
    preprocess_line(prep, 0);
    prep->line = NULL;
    while (deque_len(&prep->lookahead)
        && deque_back(&prep->lookahead).token == END)
    {
        (void) deque_pop_back(&prep->lookahead);
    }
}

INTERNAL struct token next(struct preprocessor *input)
{
    if (deque_len(&input->lookahead) < 1) {
        preprocess_line(input, 1);
    }

    return deque_pop_front(&input->lookahead);
}

INTERNAL struct token peek(struct preprocessor *input)
{
    return peekn(input, 1);
}

INTERNAL struct token peekn(struct preprocessor *input, int n)
{
    assert(n > 0);
    if (deque_len(&input->lookahead) < n) {
        preprocess_line(input, n);
    }

    return deque_get(&input->lookahead, n - 1);
}

INTERNAL struct token consume(struct preprocessor *input, enum token_type type)
{
    struct token t;
    const char *str;

    t = next(input);
    if (t.token != type) {
        switch (type) {
        case IDENTIFIER:
            str = "identifier";
            break;
        case NUMBER:
            str = "number";
            break;
        case STRING:
            str = "string";
            break;
        default:
            str = str_raw(basic_token[type].d.string);
            break;
        }

        error(input, "Expected %s but got %s.", str, stringify_token(&t));
        exit(1);
    }

    return t;
}

INTERNAL void preprocess(struct preprocessor *prep, FILE *output)
{
    struct token t;

    assert(context.target == TARGET_PREPROCESS);
    while ((t = next(prep)).token != END) {
        if (t.leading_whitespace) {
            fprintf(output, "%*s", t.leading_whitespace, " ");
        }

        switch (t.token) {
        case NUMBER:
            assert(0);
            break;
        case PREP_STRING:
        case STRING:
            putc('\"', output);
            fprintf(output, "%s", str_raw(t.d.string));
            putc('\"', output);
            break;
        case PREP_CHAR:
            putc('\'', output);
            fprintf(output, "%s", str_raw(t.d.string));
            putc('\'', output);
            break;
        default:
            fprintf(output, "%s", str_raw(t.d.string));
            break;
        }
    }
}
