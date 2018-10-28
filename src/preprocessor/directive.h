#ifndef DIRECTIVE_H
#define DIRECTIVE_H

#include "input.h"
#include "macro.h"
#include <lacc/token.h>

EXTERNAL struct token
    ident__include,
    ident__defined,
    ident__define,
    ident__ifndef,
    ident__ifdef,
    ident__undef,
    ident__elif,
    ident__endif,
    ident__error,
    ident__pragma,
    ident__Pragma;

enum state {
    /*
     * Default state, inside an active #if, #elif, #else, #ifdef, or
     * #ifndef directive.
     */
    BRANCH_LIVE,
    /*
     * A Previous branch in #if, #elif chain was taken. Everything
     * up until #endif is dead code, and new #elif directives will not
     * be computed.
     */
    BRANCH_DISABLED,
    /*
     * Dead code. New #else or #elif directives become live if evaluated
     * to true.
     */
    BRANCH_DEAD
};

/*
 * Preprocess a line starting with a '#' directive. Borrows ownership of
 * input. Assume input is END terminated.
 */
INTERNAL void preprocess_directive(
    struct preprocessor *prep,
    TokenArray *array);

/* Non-zero iff currently not inside a false #if directive. */
INTERNAL int in_active_block(struct preprocessor *prep);

#endif
