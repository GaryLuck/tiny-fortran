#ifndef AST_H
#define AST_H

typedef enum {
    /* Top-level units */
    NODE_PROGRAM,
    NODE_FUNC_DEF,
    NODE_SUB_DEF,

    /* Statements */
    NODE_ASSIGN,
    NODE_IF,
    NODE_DO_LOOP,
    NODE_DO_WHILE,
    NODE_PRINT,
    NODE_READ,
    NODE_CALL,
    NODE_RETURN,
    NODE_STOP,
    NODE_EXIT,
    NODE_CYCLE,
    NODE_DECL,        /* INTEGER :: var1, var2 */

    /* Expressions */
    NODE_INT_LIT,
    NODE_STR_LIT,
    NODE_VAR_REF,
    NODE_BINOP,
    NODE_UNARY,
    NODE_FUNC_CALL,

    /* Helper */
    NODE_BLOCK        /* list of statements */
} NodeType;

typedef enum {
    OP_ADD, OP_SUB, OP_MUL, OP_DIV,
    OP_EQ, OP_NE, OP_LT, OP_GT, OP_LE, OP_GE,
    OP_AND, OP_OR, OP_NOT, OP_NEG
} OpType;

typedef enum {
    INTENT_NONE,
    INTENT_IN,
    INTENT_INOUT
} IntentType;

/* Parameter descriptor for function/subroutine definitions */
typedef struct {
    char *name;
    IntentType intent;
} ParamDef;

typedef struct ASTNode ASTNode;

struct ASTNode {
    NodeType type;
    int line;
    union {
        /* NODE_PROGRAM */
        struct { char *name; ASTNode **stmts; int nstmts; } program;

        /* NODE_FUNC_DEF */
        struct {
            char *name;
            ParamDef *params; int nparams;
            ASTNode **body; int nbody;
        } func_def;

        /* NODE_SUB_DEF */
        struct {
            char *name;
            ParamDef *params; int nparams;
            ASTNode **body; int nbody;
        } sub_def;

        /* NODE_ASSIGN: target[(index)] = value */
        struct { char *target; ASTNode *value; ASTNode *index; } assign;

        /* NODE_IF */
        struct {
            ASTNode *cond;
            ASTNode **then_body; int nthen;
            ASTNode *else_branch; /* NULL, another NODE_IF (elseif), or NODE_BLOCK */
        } if_stmt;

        /* NODE_DO_LOOP: DO var = start, end [, step] */
        struct {
            char *var;
            ASTNode *start; ASTNode *end; ASTNode *step; /* step may be NULL */
            ASTNode **body; int nbody;
        } do_loop;

        /* NODE_DO_WHILE */
        struct {
            ASTNode *cond;
            ASTNode **body; int nbody;
        } do_while;

        /* NODE_PRINT: PRINT *, items */
        struct { ASTNode **items; int nitems; } print;

        /* NODE_READ: READ *, vars — indices[i] non-NULL for array element */
        struct { char **vars; int nvars; ASTNode **indices; } read;

        /* NODE_CALL: CALL sub(args) */
        struct { char *name; ASTNode **args; int nargs; } call;

        /* NODE_DECL: INTEGER [, INTENT(x)] :: var1, var2, arr(10) */
        struct { char **names; int nnames; IntentType intent; int *sizes; } decl;

        /* NODE_INT_LIT */
        struct { int value; } int_lit;

        /* NODE_STR_LIT */
        struct { char *value; } str_lit;

        /* NODE_VAR_REF */
        struct { char *name; } var_ref;

        /* NODE_BINOP */
        struct { OpType op; ASTNode *left; ASTNode *right; } binop;

        /* NODE_UNARY */
        struct { OpType op; ASTNode *operand; } unary;

        /* NODE_FUNC_CALL */
        struct { char *name; ASTNode **args; int nargs; } func_call;

        /* NODE_BLOCK */
        struct { ASTNode **stmts; int nstmts; } block;
    } data;
};

/* Constructors */
ASTNode *ast_program(const char *name, ASTNode **stmts, int nstmts, int line);
ASTNode *ast_func_def(const char *name, ParamDef *params, int nparams,
                      ASTNode **body, int nbody, int line);
ASTNode *ast_sub_def(const char *name, ParamDef *params, int nparams,
                     ASTNode **body, int nbody, int line);
ASTNode *ast_assign(const char *target, ASTNode *index, ASTNode *value, int line);
ASTNode *ast_if(ASTNode *cond, ASTNode **then_body, int nthen,
                ASTNode *else_branch, int line);
ASTNode *ast_do_loop(const char *var, ASTNode *start, ASTNode *end,
                     ASTNode *step, ASTNode **body, int nbody, int line);
ASTNode *ast_do_while(ASTNode *cond, ASTNode **body, int nbody, int line);
ASTNode *ast_print(ASTNode **items, int nitems, int line);
ASTNode *ast_read(char **vars, ASTNode **indices, int nvars, int line);
ASTNode *ast_call(const char *name, ASTNode **args, int nargs, int line);
ASTNode *ast_return(int line);
ASTNode *ast_stop(int line);
ASTNode *ast_exit(int line);
ASTNode *ast_cycle(int line);
ASTNode *ast_decl(char **names, int nnames, IntentType intent, int *sizes, int line);
ASTNode *ast_int_lit(int value, int line);
ASTNode *ast_str_lit(const char *value, int line);
ASTNode *ast_var_ref(const char *name, int line);
ASTNode *ast_binop(OpType op, ASTNode *left, ASTNode *right, int line);
ASTNode *ast_unary(OpType op, ASTNode *operand, int line);
ASTNode *ast_func_call(const char *name, ASTNode **args, int nargs, int line);
ASTNode *ast_block(ASTNode **stmts, int nstmts, int line);

/* Memory management */
void ast_free(ASTNode *node);

#endif
