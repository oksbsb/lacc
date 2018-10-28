#ifndef STRTAB_H
#define STRTAB_H

#include <lacc/hash.h>
#include <lacc/string.h>
#include <lacc/token.h>
#include <lacc/type.h>

#include <stddef.h>

struct strtab {
    struct hash_table tab;

    /* Buffer used to concatenate strings before registering them. */
    char *catbuf;
    size_t catlen;
};

/*
 * Clear data contained in string table, or initialize a new all-zero
 * structure.
 *
 * The same table can be initialized multiple times to re-use memory.
 */
INTERNAL void strtab_init(struct strtab *strtab);

/*
 * Register a string and store it internally, allocating a new copy if
 * needed. Manages memory ownership for all string constants used at
 * runtime.
 */
INTERNAL String str_register(
    struct strtab *strtab,
    const char *str,
    size_t len);

/* Concatenate two strings together. */
INTERNAL String str_cat(struct strtab *strtab, String a, String b);

/*
 * Free memory used for string table. Must be called once after the
 * final strtab_init.
 */
INTERNAL void strtab_destroy(struct strtab *strtab);

#endif
