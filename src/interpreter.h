#ifndef INTERPRETER_H
#define INTERPRETER_H

#include "ast.h"

typedef struct Interpreter Interpreter;

Interpreter *interp_new(void);
void interp_free(Interpreter *interp);

/* Register top-level units (PROGRAM, FUNCTION, SUBROUTINE) */
void interp_register(Interpreter *interp, ASTNode **units, int nunits);

/* Run the PROGRAM unit */
int interp_run(Interpreter *interp);

#endif
