#ifndef INPUT_H
#define INPUT_H

#include "strtab.h"
#include <lacc/array.h>
#include <lacc/string.h>

#include <stdio.h>
#include <stdlib.h>

/*
 * Paths specified with -I, append to list of directories to search when
 * resolving includes.
 */
INTERNAL int add_include_search_path(const char *path);

/*
 * Initialize with root file name, and store relative path to resolve
 * later includes. Passing NULL defaults to taking input from stdin.
 */
INTERNAL struct input *input_open(const char *path);

/*
 * Yield next line of input, with initial preprocessing applied. This
 * includes:
 *
 *     - Join line continuations
 *     - Replace comments with a single space
 *     - Substitute trigraphs with corresponding character
 *
 */
INTERNAL const char *getprepline(struct input *input);

/* Push new include file. Return 1 iff successful. */
INTERNAL int include_file(
    struct input *input,
    struct strtab *strtab,
    const char *path);

/* Push new include file. Return 1 iff successful. */
INTERNAL int include_system_file(
    struct input *input,
    struct strtab *strtab,
    const char *path);

/* Path of file and line number that was last read. */
INTERNAL int input_pos(const struct input *input, String *path, int *line);

/* Override current line. */
INTERNAL void input_set_line(struct input *input, int line);

/* Override current file. */
INTERNAL void input_set_file(struct input *input, String path);

/*
 * Free resources used for reading input. Must be called once for each
 * input created.
 */
INTERNAL void input_close(struct input *input);

/*
 * Free static resources used for reading input. Must be called once
 * before program exits.
 */
INTERNAL void input_finalize(void);

#endif
