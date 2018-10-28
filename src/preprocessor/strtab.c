#if !AMALGAMATION
# define INTERNAL
# define EXTERNAL extern
#endif
#include "strtab.h"

#include <assert.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>

#define STRTAB_SIZE 1024

/*
 * Every unique string encountered, being identifiers or literals, is
 * kept for the lifetime of the program. To save allocations, store the
 * raw string buffer in the same allocation as the struct.
 *
 *  _________ String ________    ________ const char [] ________
 * |                          | |                               |
 * [ <len> | <ptr to data>    ] [ 'H', 'e', 'l', 'l', 'o', '\0' ]
 */
static void *str_hash_add(void *ref)
{
    String *s;
    char *buffer;
    unsigned short l;

    s = (String *) ref;
    l = s->p.len;
    buffer = malloc(sizeof(String) + l + 1);
    buffer[sizeof(String) + l] = '\0';
    memcpy(buffer + sizeof(String), s->p.str, l);
    s = (String *) buffer;
    s->p.str = buffer + sizeof(*s);
    s->p.len = l;
    return s;
}

static String str_hash_key(void *ref)
{
    String *str = (String *) ref;
    return *str;
}

INTERNAL void strtab_init(struct strtab *strtab)
{
    if (strtab->tab.table) {
        hash_clear(&strtab->tab);
    } else {
        hash_init(
            &strtab->tab,
            STRTAB_SIZE,
            str_hash_key,
            str_hash_add,
            free);
    }
}

INTERNAL void strtab_destroy(struct strtab *strtab)
{
    free(strtab->catbuf);
    hash_destroy(&strtab->tab);
    memset(strtab, 0, sizeof(*strtab));
}

INTERNAL String str_register(
    struct strtab *strtab,
    const char *str,
    size_t len)
{
    String data = {0}, *ref;
    assert(len >= 0);

    if (len < SHORT_STRING_LEN) {
        memcpy(data.a.str, str, len);
        data.a.len = len;
        ref = &data;
    } else {
        data.p.str = str;
        data.p.len = len;
        ref = hash_insert(&strtab->tab, &data);
    }

    return *ref;
}

INTERNAL String str_cat(struct strtab *strtab, String a, String b)
{
    size_t len;

    len = a.len + b.len;
    if (len > strtab->catlen) {
        strtab->catlen = len;
        strtab->catbuf = realloc(strtab->catbuf, strtab->catlen);
    }

    memcpy(strtab->catbuf, str_raw(a), a.len);
    memcpy(strtab->catbuf + a.len, str_raw(b), b.len);
    return str_register(strtab, strtab->catbuf, len);
}
