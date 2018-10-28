#ifndef PREPROCESS_H
#define PREPROCESS_H

#include "directive.h"
#include "input.h"
#include "strtab.h"
#include <lacc/deque.h>
#include <lacc/hash.h>

#include <stdio.h>

struct preprocessor {
    struct input *input;

    /*
     * Current line from input stream, pointing to next position to
     * tokenize.
     */
    const char *line;

    /* Number of errors encountered. */
    int errors;

    /*
     * Buffer of preprocessed tokens, ready to be consumed by the parser.
     * Filled lazily on calls to peek(0), peekn(1) and next(0).
     */
    deque_of(struct token) lookahead;

    /* Hash table with macro definitions. */
    struct hash_table macros;

    /* Hash table with strings. */
    struct strtab strtab;

    /*
     * Keep stack of branch conditions for #if, #elif and #endif. Push
     * and pop according to current and parent state, and result of
     * evaluating expressions.
     */
    array_of(enum state) branch_stack;

    /* Keep track of arrays being recycled. */
    array_of(TokenArray) arrays;
    array_of(ExpandStack) stacks;
};

/*
 * Initialize data structures used for preprocessing, with given source
 * input stream.
 */
INTERNAL void preprocess_init(struct preprocessor *prep, struct input *input);

/*
 * Preprocess a single line, adding any resulting tokens to the
 * lookahead buffer.
 */
INTERNAL void inject_line(struct preprocessor *prep, const char *line);

/*
 * Output preprocessed input to provided stream, toggled by -E program
 * option.
 */
INTERNAL void preprocess(struct preprocessor *prep, FILE *output);

/* Free memory used for preprocessing. */
INTERNAL void preprocess_finalize(struct preprocessor *prep);

#endif
