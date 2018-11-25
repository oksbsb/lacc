#ifndef EXPRESSION_H
#define EXPRESSION_H

#include <lacc/ir.h>

INTERNAL struct block *expression(
	struct preprocessor *input,
	struct definition *def,
	struct block *block);

INTERNAL struct var constant_expression(struct preprocessor *input);

INTERNAL struct block *assignment_expression(
	struct preprocessor *input,
	struct definition *def,
	struct block *block);

/*
 * Free memory used to hold function arguments.
 *
 * Should be called exactly once before exiting.
 */
INTERNAL void clear_argument_lists(void);

#endif
