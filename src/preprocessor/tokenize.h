#ifndef TOKENIZE_H
#define TOKENIZE_H

#include "strtab.h"
#include <lacc/token.h>

/*
 * Table is indexed by ASCII value, which is also assigned to
 * corresponding token type. To get a token of a particular type,
 * access basic_token[type].
 */
EXTERNAL const struct token basic_token[128];

/*
 * Transform preprocessing number to numeric literal, parsing the string
 * representation to a typed number.
 *
 * This is done as a last step in preprocessing before handing the token
 * over to the parser.
 */
INTERNAL struct token convert_preprocessing_number(
	struct preprocessor *prep,
	struct token t);

/*
 * Transform preprocessing string by substituting all escape sequences
 * by the corresponding character.
 *
 * This is done as a last step in preprocessing before handing the token
 * over to the parser.
 */
INTERNAL struct token convert_preprocessing_string(
    struct preprocessor *prep,
    struct token t);

/*
 * Transform preprocessing character to numeric literal, converting the
 * string representation to an integer value stored in a NUMBER token.
 *
 * This is done as a last step in preprocessing before handing the token
 * over to the parser.
 */
INTERNAL struct token convert_preprocessing_char(
	struct preprocessor *prep,
	struct token t);

/*
 * Parse and return next preprocessing token from given line. Assume
 * comments are removed and line continuations are applied. endptr is
 * set to point to one index past the last character producing the
 * token.
 */
INTERNAL struct token tokenize(
    struct preprocessor *prep,
    const char *in,
    const char **endptr);

#endif
