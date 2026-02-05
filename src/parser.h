#ifndef PARSER_H
#define PARSER_H

#include "ast.h"

/* Parse result: array of top-level units (PROGRAM, FUNCTION, SUBROUTINE) */
typedef struct {
    ASTNode **units;
    int nunits;
    char *error;   /* NULL if no error */
    int error_line;
} ParseResult;

ParseResult parse(const char *source);
void parse_result_free(ParseResult *result);

#endif
