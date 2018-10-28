#if !AMALGAMATION
# define INTERNAL
# define EXTERNAL extern
#endif
#include "input.h"
#include "strtab.h"
#include <lacc/context.h>

#include <assert.h>
#include <ctype.h>
#include <string.h>
#include <unistd.h>

#define FILE_BUFFER_SIZE 4096

struct source {
    FILE *file;

    /*
     * Total capacity of the line buffer is represented by size. The
     * number of characters already handled, a prefix, is 'processed'.
     * Read is the number of valid characters in the buffer. The
     * processed count grows on successive calls towards the read
     * number. When all read characters are processed, or the remaining
     * interval between (processed, read) does not contain a full line,
     * rewind the buffer, or increase if necessary.
     */
    char *buffer;
    size_t size, processed, read;

    /* Full path, or relative to invocation directory. */
    String path;

    /*
     * Number of characters into path occupied by directory, not
     * including the last slash.
     */
    int dirlen;

    /* Current line. */
    int line;
};

struct input {
    /*
     * Keep stack of file descriptors as resolved by includes. Push and
     * pop from the end of the list.
     */
    array_of(struct source) source_stack;

    /* Buffer for reading lines. */
    char *rline;
    size_t rlen;
};

/* List of directories to search on resolving include directives. */
static array_of(const char *) search_path_list;

static struct source *current_file(const struct input *input)
{
    assert(array_len(&input->source_stack));
    return &array_back(&input->source_stack);
}

INTERNAL int input_pos(const struct input *input, String *path, int *line)
{
    struct source *file;

    file = current_file(input);
    *path = file->path;
    *line = file->line;
    return 0;
}

INTERNAL void input_set_line(struct input *input, int line)
{
    struct source *file;

    file = current_file(input);
    file->line = line;
}

INTERNAL void input_set_file(struct input *input, String path)
{
    struct source *file;

    file = current_file(input);
    file->path = path;
}

static void push_file(struct input *input, struct source source)
{
    assert(source.file);
    assert(source.path.len);

    source.buffer = malloc(FILE_BUFFER_SIZE);
    source.size = FILE_BUFFER_SIZE;
    array_push_back(&input->source_stack, source);
}

static int pop_file(struct input *input)
{
    unsigned len;
    struct source source;

    len = array_len(&input->source_stack);
    if (len) {
        source = array_pop_back(&input->source_stack);
        if (source.file != stdin) {
            fclose(source.file);
        }

        free(source.buffer);
        if (len - 1) {
            return 1;
        }
    }

    return EOF;
}

INTERNAL void input_close(struct input *input)
{
    while (pop_file(input) != EOF)
        ;

    assert(!array_len(&input->source_stack));
    array_clear(&input->source_stack);
    free(input->rline);
    free(input);
}

INTERNAL void input_finalize(void)
{
    array_clear(&search_path_list);
}

static size_t path_dirlen(const char *path)
{
    const char *rchr = strrchr(path, '/');
    return (rchr) ? rchr - path : 0;
}

static char *create_path(
    const char *path,
    size_t dirlen,
    const char *name,
    struct strtab *strtab)
{
    size_t required, namelen;

    namelen = strlen(name);
    required = dirlen + namelen + 2;
    if (required > strtab->catlen) {
        strtab->catlen = required;
        strtab->catbuf = realloc(strtab->catbuf, strtab->catlen);
    }

    strncpy(strtab->catbuf, path, dirlen);
    strtab->catbuf[dirlen] = '/';
    strncpy(strtab->catbuf + dirlen + 1, name, namelen + 1);
    return strtab->catbuf;
}

INTERNAL int include_file(
    struct input *input,
    struct strtab *strtab,
    const char *name)
{
    const char *path;
    struct source *file;
    struct source source = {0};

    /*
     * Construct path by combining current directory and include name,
     * which itself can include folders. Except for root level, where
     * the whole name is already specified.
     */
    file = current_file(input);
    if (file->dirlen && name[0] != '/') {
        path = create_path(str_raw(file->path), file->dirlen, name, strtab);
    } else {
        path = name;
    }

    source.file = fopen(path, "r");
    if (source.file) {
        source.path = str_register(strtab, path, strlen(path));
        source.dirlen = path_dirlen(path);
        push_file(input, source);
        return 1;
    }

    return include_system_file(input, strtab, name);
}

INTERNAL int include_system_file(
    struct input *input,
    struct strtab *strtab,
    const char *name)
{
    struct source source = {0};
    const char *path;
    size_t dirlen;
    int i;

    for (i = 0; i < array_len(&search_path_list); ++i) {
        path = array_get(&search_path_list, i);
        dirlen = strlen(path);
        while (path[dirlen - 1] == '/') {
            dirlen--;
            assert(dirlen);
        }

        path = create_path(path, dirlen, name, strtab);
        source.file = fopen(path, "r");
        if (source.file) {
            source.path = str_register(strtab, path, strlen(path));
            source.dirlen = path_dirlen(path);
            break;
        }
    }

    if (source.file) {
        push_file(input, source);
        return 1;
    }

    return 0;
}

INTERNAL int add_include_search_path(const char *path)
{
    array_push_back(&search_path_list, path);
    return 0;
}

INTERNAL struct input *input_open(const char *path)
{
    const char *sep;
    struct input *input;
    struct source source = {0};

    input = calloc(1, sizeof(*input));
    input->rlen = FILE_BUFFER_SIZE;
    input->rline = calloc(input->rlen, sizeof(*input->rline));

    if (path) {
        sep = strrchr(path, '/');
        source.path = str_init(path);
        source.file = fopen(path, "r");
        if (sep) {
            source.dirlen = sep - path;
        }
        if (!source.file) {
            error(NULL, "Unable to open file %s.", path);
            exit(1);
        }
    } else {
        source.file = stdin;
        source.path = str_init("<stdin>");
    }

    push_file(input, source);
    return input;
}

/*
 * Consume input until encountering end of comment. Return number of
 * characters read, or 0 if end of input reached.
 *
 * This must also handle line continuations, which logically happens
 * before replacing comments with whitespace.
 */
static size_t read_comment(const char *line, int *linecount)
{
    char c;
    const char *ptr;

    ptr = line;
    do {
        c = *ptr++;
        if (c == '*') {
            while (*ptr == '\\' && ptr[1] == '\n') {
                *linecount += 1;
                ptr += 2;
            }
            if (*ptr == '/') {
                return ptr + 1 - line;
            }
        } else if (c == '\n') {
            *linecount += 1;
        }
    } while (c != '\0');
    return 0;
}

/*
 * Read single line comment ending at the first newline. Return number
 * of characters read, or 0 if end of input reached.
 */
static size_t read_line_comment(const char *line, int *linecount)
{
    char c;
    const char *ptr;

    ptr = line;
    do {
        c = *ptr++;
        if (c == '\\' && *ptr == '\n') {
            *linecount += 1;
            ptr++;
        } else if (c == '\n') {
            return ptr - line;
        }
    } while (c != '\0');
    return 0;
}

/*
 * Read trigraph character, produced by pattern '??X', where X is the
 * input.
 */
static char read_trigraph(char c)
{
    switch (c) {
    case '=':  return '#';
    case '(':  return '[';
    case '/':  return '\\';
    case ')':  return ']';
    case '\'': return '^';
    case '<':  return '{';
    case '!':  return '|';
    case '>':  return '}';
    case '-':  return '~';
    default:
        return 0;
    }
}

/*
 * Consume characters forming a quoted string or character literal.
 *
 * Initial preprocessing must consider quoted text because it is allowed
 * to have embedded comments, which should not be replaced by single
 * whitespace.
 *
 * Handle trigraphs and line continuations as in normal input.
 */
static size_t read_literal(const char *line, char **buf, int *lines)
{
    char c;
    char *ptr;
    int count;
    const char *end, q = *line;

    end = line;
    ptr = *buf;
    assert(q == '"' || q == '\'');
    *ptr++ = *end++;

    while ((c = *end) != '\0') {
        switch (c) {
        case '\n':
            if (ptr[-1] == '\\') {
                *lines += 1;
                ptr -= 1;
                end += 1;
                continue;
            } else {
                error(NULL, "Unexpected newline in literal.");
                exit(1);
            }
            break;
        case '?':
            if (end[1] == '?') {
                c = read_trigraph(end[2]);
                if (c) {
                    end += 3;
                    *ptr++ = c;
                    continue;
                }
            }
            break;
        case '\'':
        case '"':
            if (c == q) {
                count = 0;
                while (ptr[-(count + 1)] == '\\') {
                    count++;
                }
                if (count % 2 == 0) {
                    *ptr++ = *end++;
                    *buf = ptr;
                    return end - line;
                }
            }
        default:
            break;
        }

        *ptr++ = *end++;
    }

    return 0;
}

/*
 * Read initial part of line, until forming a complete source line ready
 * for tokenization. Store the result in rline, with the following
 * mutations done:
 *
 *  - Join line continuations.
 *  - Replace comments with a single whitespace character.
 *  - Replace trigraph sequence with corresponding character.
 *
 * Return non-zero number of consumed characters, or 0 if input buffer
 * does not contain a complete line. Note that the source code line can
 * be smaller than this number, by any of the transformations removing
 * characters.
 */
static size_t read_line(struct source *fn, char *ptr)
{
    char c;
    int lines;
    size_t count, len;
    const char *end, *line;

    line = fn->buffer + fn->processed;
    len = fn->read - fn->processed;

    lines = 0;
    assert(ptr[-1] == '\0');
    end = line;
    do {
        switch (*end) {
        case '\n':
            fn->line += lines + 1;
            *ptr++ = '\0';
            return end + 1 - line;
        case '\\':
            if (end[1] == '\n') {
                lines += 1;
                end += 2;
                continue;
            }
            break;
        case '?':
            if (end[1] == '?') {
                c = read_trigraph(end[2]);
                if (c) {
                    end += 3;
                    *ptr++ = c;
                    continue;
                }
            }
            break;
        case '\'':
        case '"':
            count = read_literal(end, &ptr, &lines);
            end += count;
            if (!count) {
                return 0;
            }
            continue;
        case '/':
            if (ptr[-1] == '/') {
                count = read_line_comment(end + 1, &lines);
                if (!count) {
                    return 0;
                }
                ptr[-1] = '\0';
                fn->line += lines + 1;
                return end + count + 1 - line;
            }
            break;
        case '*':
            if (ptr[-1] == '/') {
                count = read_comment(end + 1, &lines);
                if (!count) {
                    return 0;
                }
                end += count + 1;
                ptr[-1] = ' ';
                continue;
            }
            break;
        }
        *ptr++ = *end++;
    } while (end - line < len);

    return 0;
}

/*
 * Read the next line from file input, doing initial pre-preprocessing.
 */
static char *initial_preprocess_line(
    struct input *input,
    struct source *fn)
{
    size_t added, len;

    assert(fn->buffer);
    assert(fn->processed <= fn->read);
    assert(fn->read < fn->size);

    do {
        if (fn->processed == fn->read || !fn->processed) {
            if (feof(fn->file)) {
                if (fn->read > fn->processed) {
                    error(NULL, "Unable to process the whole input.");
                    exit(1);
                }
                return NULL;
            }
            if (!fn->processed) {
                fn->read += fread(
                    fn->buffer + fn->read,
                    sizeof(char),
                    fn->size - fn->read - 1,
                    fn->file);
            } else {
                fn->read = fread(
                    fn->buffer,
                    sizeof(char),
                    fn->size - 1,
                    fn->file);
            }
            fn->processed = 0;
            fn->buffer[fn->read] = '\0';
            if (feof(fn->file)) {
                if (!fn->read) {
                    return NULL;
                }
                if (fn->buffer[fn->read - 1] != '\n') {
                    error(NULL, "Missing newline at end of file.");
                    fn->buffer[fn->read] = '\n';
                }
            }
        }

        assert(fn->processed < fn->read);
        len = fn->read - fn->processed;
        if (len >= input->rlen) {
            input->rlen = len + 1;
            input->rline = realloc(input->rline, input->rlen);
        }

        added = read_line(fn, input->rline + 1);
        if (!added) {
            if (!fn->processed) {
                fn->size += FILE_BUFFER_SIZE;
                fn->buffer = realloc(fn->buffer, fn->size);
            } else {
                memmove(
                    fn->buffer,
                    fn->buffer + fn->processed,
                    fn->read - fn->processed);
                assert(fn->read > fn->processed);
                fn->read -= fn->processed;
                fn->processed = 0;
            }
        }
    } while (!added);

    fn->processed += added;
    return input->rline + 1;
}

/*
 * Yield next line ready for further preprocessing. Joins continuations,
 * and replaces comments with a single space. Line implicitly ends with
 * a single newline character ('\n'), but it is not included.
 */
INTERNAL const char *getprepline(struct input *input)
{
    struct source *source = NULL;
    const char *line;

    do {
        if (!array_len(&input->source_stack)) {
            return NULL;
        }

        source = &array_back(&input->source_stack);
        line = initial_preprocess_line(input, source);

    } while (!line && pop_file(input) != EOF);

    if (context.verbose && line) {
        verbose("(%s, %d): `%s`", str_raw(source->path), source->line, line);
    }

    return line;
}
