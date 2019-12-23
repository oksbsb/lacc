#ifndef TOKEN_H
#define TOKEN_H
#if !defined(INTERNAL) || !defined(EXTERNAL)
# error Missing amalgamation macros
#endif

#include "string.h"
#include "type.h"

/*
 * Map token type to corresponding numerical ascii value where possible,
 * and fit the remaining tokens in between.
 */
enum token_type {
    END = 0,                /*  $ */
    AUTO,
    BREAK,
    CASE,
    CHAR,
    CONST,
    CONTINUE,
    DEFAULT,
    DO,
    DOUBLE,
    NEWLINE = '\n',
    ELSE,
    ENUM,
    EXTERN,
    FLOAT,
    FOR,
    GOTO,
    IF,
    INT,
    LONG,
    REGISTER,
    RETURN,
    SHORT,
    SIGNED,
    SIZEOF,
    STATIC,
    STRUCT,
    SWITCH,
    TYPEDEF,
    UNION,
    UNSIGNED,
    VOID,
    INLINE,

    NOT = '!',
    VOLATILE,
    HASH = '#',
    DOLLAR = '$',
    MODULO = '%',
    AND = '&',
    WHILE,

    OPEN_PAREN = '(',
    CLOSE_PAREN = ')',
    STAR = '*',
    PLUS = '+',
    COMMA = ',',
    MINUS = '-',
    DOT = '.',
    SLASH = '/',
    RESTRICT,
    ALIGNOF,
    BOOL,

    STATIC_ASSERT = BOOL + 6,

    COLON = ':',
    SEMICOLON = ';',
    LT = '<',
    ASSIGN = '=',
    GT = '>',
    QUESTION = '?',
    AMPERSAND = '@',
    LOGICAL_OR,             /* || */
    LOGICAL_AND,            /* && */
    LEQ,                    /* <= */
    GEQ,                    /* >= */
    EQ,                     /* == */
    NEQ,                    /* != */
    ARROW,                  /* -> */
    INCREMENT,              /* ++ */
    DECREMENT,              /* -- */
    LSHIFT,                 /* << */
    RSHIFT,                 /* >> */
    MUL_ASSIGN,             /* *= */
    DIV_ASSIGN,             /* /= */
    MOD_ASSIGN,             /* %= */
    PLUS_ASSIGN,            /* += */
    MINUS_ASSIGN,           /* -= */
    LSHIFT_ASSIGN,          /* <<= */
    RSHIFT_ASSIGN,          /* >>= */
    AND_ASSIGN,             /* &= */
    XOR_ASSIGN,             /* ^= */
    OR_ASSIGN,              /* |= */
    TOKEN_PASTE,            /* ## */
    DOTS,                   /* ... */

    OPEN_BRACKET = '[',
    BACKSLASH = '\\',
    CLOSE_BRACKET = ']',
    XOR = '^',
    BACKTICK = '`',
    OPEN_CURLY = '{',
    OR = '|',
    CLOSE_CURLY = '}',
    NEG = '~',

    /* Non-standard keywords. */
    ASM = 0x68,

    /*
     * The remaining tokens do not correspond to any fixed string, and
     * are placed at arbitrary locations.
     */
    NUMBER = 116,
    IDENTIFIER,
    STRING,

    /*
     * Pseudo-token representing parameter substitution in macros. Has
     * an immediate integer value referring to the parameter index.
     */
    PARAM,

    /*
     * Preprocessing token representing a number. Valid strings include
     * a superset of numeric constants in C. A conversion to NUMBER
     * token is done before handed to parser.
     */
    PREP_NUMBER,
    PREP_CHAR,
    PREP_STRING
};

/*
 * Hold an immediate numeric value. Associated type is used to determine
 * which element is valid.
 */
union value {
    unsigned QWORD u;
    signed QWORD i;
    float f;
    double d;
    long double ld;
};

INTERNAL union value convert(union value val, Type from, Type to);

/*
 * Representation of token; used in preprocessing, and interface to
 * parser.
 *
 * Tokens keep track of typed numbers, to capture difference between
 * literals like 1 and 1ul. Type should always correspond to one of the
 * basic integer types.
 */
struct token {
    enum token_type token;
    unsigned int leading_whitespace : 16;
    unsigned int is_expandable : 1;
    unsigned int disable_expand : 1;
    Type type;
    union {
        String string;
        union value val;
    } d;
};

/* Peek lookahead of 1. */
INTERNAL struct token peek(void);

/* Peek lookahead of n. */
INTERNAL struct token peekn(int n);

/* Consume and return next token. */
INTERNAL struct token next(void);

/* Consume and return next token, or fail of not of expected type. */
INTERNAL struct token consume(enum token_type type);

#endif
