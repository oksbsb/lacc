#ifndef MACRO_H
#define MACRO_H

#include <lacc/array.h>
#include <lacc/context.h>
#include <lacc/hash.h>
#include <lacc/token.h>

typedef array_of(struct token) TokenArray;
typedef array_of(String) ExpandStack;

struct preprocessor;

/* Get empty token array, possibly already allocated with capacity. */
INTERNAL TokenArray get_token_array(struct preprocessor *prep);

/* Release token array previously aquired by get_token_array. */
INTERNAL void release_token_array(struct preprocessor *prep, TokenArray list);

/* Internal representation of a macro definition. */
struct macro {
    String name;

    enum {
        OBJECT_LIKE,
        FUNCTION_LIKE
    } type;

    /* Number of parameters required for substitution. */
    int params;

    unsigned int is__line__ : 1;
    unsigned int is__file__ : 1;
    unsigned int is_vararg : 1;
    unsigned int is_added : 1;

    /*
     * A substitution is either a token or a parameter, and parameters
     * are represented by PARAM tokens with an integer index between
     * 0 and params.
     */
    TokenArray replacement;

    /* Reference back to preprocessor owning this macro. */
    struct preprocessor *owner;
};

/*
 * Initialize hash table used for macro definitions. Recycle buffers
 * between input files.
 */
INTERNAL void macro_init(struct hash_table *table);

/*
 * Define macros that are intrinsic to the compiler, or mandated by the
 * standard.
 */
INTERNAL void register_builtin_definitions(
    struct preprocessor *prep,
    enum cstd version);

/* Stringify a list of tokens, returning a new token of type STRING. */
INTERNAL struct token stringify(
    struct preprocessor *prep,
    const TokenArray *list);

/*
 * Add macro definition. Takes ownership of any dynamically allocated
 * replacement list.
 */
INTERNAL void define(struct preprocessor *prep, struct macro macro);

/*
 * Remove macro definition corresponding to identifier. If the name has
 * not previously been defined, this is a no-op.
 */
INTERNAL void undef(struct preprocessor *prep, String name);

/* Look up definition of identifier, or NULL if not defined. */
INTERNAL const struct macro *macro_definition(
    struct preprocessor *prep,
    String name);

/*
 * Expand a list of tokens, replacing any macro definitions. Mutates
 * input list as necessary. Return non-zero if any macro was expanded.
 */
INTERNAL int expand(struct preprocessor *prep, TokenArray *list);

/*
 * Compare tokens for equality, returing 0 iff of same type and value.
 */
INTERNAL int tok_cmp(struct token a, struct token b);

#endif
