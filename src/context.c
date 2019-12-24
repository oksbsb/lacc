#if !AMALGAMATION
# define INTERNAL
# define EXTERNAL extern
#endif
#include "parser/typetree.h"
#include "preprocessor/input.h"
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
                    n += fprintf(stream, "%llu", va_arg(ap, unsigned QWORD));
                    break;
                case 'd':
                    n += fprintf(stream, "%lld", va_arg(ap, QWORD));
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
    if (context.verbose) {
        va_list args;
        va_start(args, format);
        vfprintf_cc(stdout, format, args);
        fputc('\n', stdout);
        va_end(args);
    }
}

INTERNAL void warning(const char *format, ...)
{
    va_list args;
    if (!context.suppress_warning) {
        va_start(args, format);
        fprintf(
            stderr,
            "(%s, %d) warning: ",
            str_raw(current_file_path),
            current_file_line);
        vfprintf_cc(stderr, format, args);
        fputc('\n', stderr);
        va_end(args);
    }
}

INTERNAL void error(const char *format, ...)
{
    va_list args;

    context.errors++;
    va_start(args, format);
    fprintf(
        stderr,
        "(%s, %d) error: ",
        str_raw(current_file_path),
        current_file_line);
    vfprintf_cc(stderr, format, args);
    fputc('\n', stderr);
    va_end(args);
}

INTERNAL void fatal(const char *format, ...)
{
	va_list args;

	context.errors++;
	va_start(args, format);
	fprintf(
		stderr,
		"(%s, %d) error: ",
		str_raw(current_file_path),
		current_file_line);
	vfprintf_cc(stderr, format, args);
	fputc('\n', stderr);
	va_end(args);

	exit(1);
}
