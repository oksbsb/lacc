#ifndef CONTEXT_H
#define CONTEXT_H
#if !defined(INTERNAL) || !defined(EXTERNAL)
# error Missing amalgamation macros
#endif

#include <stddef.h>

enum target {
    TARGET_PREPROCESS,
    TARGET_IR_DOT,
    TARGET_x86_64_ASM,
    TARGET_x86_64_OBJ,
    TARGET_x86_64_EXE
};

enum cstd {
    STD_C89,
    STD_C99,
    STD_C11
};

/*
 * Global information about all translation units.
 *
 * Represents configuration given on the command line invocation.
 */
INTERNAL struct context {
    enum target target;
    enum cstd standard;

    unsigned int verbose : 1;
    unsigned int suppress_warning : 1;
    unsigned int pic : 1;            /* position independent code */
    unsigned int debug : 1;          /* Generate debug information. */
} context;

/*
 * Output diagnostics info to stdout. No-op if context.verbose is not
 * set.
 */
INTERNAL void verbose(const char *, ...);

struct preprocessor;

/*
 * Output warning to stderr. No-op if context.suppress_warning is set.
 */
INTERNAL void warning(
    const struct preprocessor *input,
    const char *format, ...);

/* Output error to stderr. */
INTERNAL void error(
    struct preprocessor *input,
    const char *format, ...);

#endif
