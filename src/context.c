#if !AMALGAMATION
# define INTERNAL
# define EXTERNAL extern
#endif
#include "parser/typetree.h"
#include "preprocessor/preprocess.h"
#include <lacc/context.h>

#include <assert.h>
#include <stdio.h>
#include <stdarg.h>

INTERNAL struct context context = {0};

/*
 * Custom implementation of printf, handling a restricted set of
 * formatters: %s, %c, %d, %lu, %ld, %%.
 *
 * In addition, have a custom formatter for objects representing a
 * compiler-internal type object.
 *
 *  %t  : Type
 *
 */
static int vfprintf_cc(FILE *stream, const char *format, va_list ap)
{
    int c, n = 0;
    if (!format) {
        return n;
    }

    while ((c = *format++) != 0) {
        if (c != '%') {
            putc(c, stream);
            n += 1;
        } else {
            c = *format++;
            switch (c) {
            default: assert(0);
            case 's':
                n += fputs(va_arg(ap, const char *), stream);
                break;
            case 'c':
                n += fprintf(stream, "%c", va_arg(ap, int));
                break;
            case 'd':
                n += fprintf(stream, "%d", va_arg(ap, int));
                break;
            case 'l':
                c = *format++;
                switch (c) {
                default: assert(0);
                case 'u':
                    n += fprintf(stream, "%lu", va_arg(ap, unsigned long));
                    break;
                case 'd':
                    n += fprintf(stream, "%ld", va_arg(ap, long));
                    break;
                }
                break;
            case 't':
                n += fprinttype(stream, va_arg(ap, Type), NULL);
                break;
            case '%':
                n += fprintf(stream, "%%");
                break;
            }
        }
    }

    return n;
}

INTERNAL void verbose(const char *format, ...)
{
    va_list args;

    if (context.verbose) {
        va_start(args, format);
        vfprintf_cc(stdout, format, args);
        fputc('\n', stdout);
        va_end(args);
    }
}

INTERNAL void warning(const struct preprocessor *prep, const char *format, ...)
{
    va_list args;
    String file;
    int line;

    if (context.suppress_warning)
        return;

    va_start(args, format);
    if (prep) {
        input_pos(prep->input, &file, &line);
        fprintf(stderr, "(%s, %d) warning: ", str_raw(file), line);
    } else {
        fprintf(stderr, "warning: ");
    }

    vfprintf_cc(stderr, format, args);
    fputc('\n', stderr);
    va_end(args);
}

INTERNAL void error(struct preprocessor *prep, const char *format, ...)
{
    va_list args;
    String file;
    int line;

    va_start(args, format);
    if (prep) {
        prep->errors++;
        input_pos(prep->input, &file, &line);
        fprintf(stderr, "(%s, %d) error: ", str_raw(file), line);
    } else {
        fprintf(stderr, "error: ");
    }

    vfprintf_cc(stderr, format, args);
    fputc('\n', stderr);
    va_end(args);
}
