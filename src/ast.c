#include <stdlib.h>
#include <string.h>
#include "ast.h"

static char *str_dup(const char *s) {
    size_t len = strlen(s);
    char *d = malloc(len + 1);
    if (d) memcpy(d, s, len + 1);
    return d;
}

static ASTNode *alloc_node(NodeType type, int line) {
    ASTNode *n = calloc(1, sizeof(ASTNode));
    n->type = type;
    n->line = line;
    return n;
}

ASTNode *ast_program(const char *name, ASTNode **stmts, int nstmts, int line) {
    ASTNode *n = alloc_node(NODE_PROGRAM, line);
    n->data.program.name = str_dup(name);
    n->data.program.stmts = stmts;
    n->data.program.nstmts = nstmts;
    return n;
}

ASTNode *ast_func_def(const char *name, ParamDef *params, int nparams,
                      ASTNode **body, int nbody, int line) {
    ASTNode *n = alloc_node(NODE_FUNC_DEF, line);
    n->data.func_def.name = str_dup(name);
    n->data.func_def.params = params;
    n->data.func_def.nparams = nparams;
    n->data.func_def.body = body;
    n->data.func_def.nbody = nbody;
    return n;
}

ASTNode *ast_sub_def(const char *name, ParamDef *params, int nparams,
                     ASTNode **body, int nbody, int line) {
    ASTNode *n = alloc_node(NODE_SUB_DEF, line);
    n->data.sub_def.name = str_dup(name);
    n->data.sub_def.params = params;
    n->data.sub_def.nparams = nparams;
    n->data.sub_def.body = body;
    n->data.sub_def.nbody = nbody;
    return n;
}

ASTNode *ast_assign(const char *target, ASTNode *value, int line) {
    ASTNode *n = alloc_node(NODE_ASSIGN, line);
    n->data.assign.target = str_dup(target);
    n->data.assign.value = value;
    return n;
}

ASTNode *ast_if(ASTNode *cond, ASTNode **then_body, int nthen,
                ASTNode *else_branch, int line) {
    ASTNode *n = alloc_node(NODE_IF, line);
    n->data.if_stmt.cond = cond;
    n->data.if_stmt.then_body = then_body;
    n->data.if_stmt.nthen = nthen;
    n->data.if_stmt.else_branch = else_branch;
    return n;
}

ASTNode *ast_do_loop(const char *var, ASTNode *start, ASTNode *end,
                     ASTNode *step, ASTNode **body, int nbody, int line) {
    ASTNode *n = alloc_node(NODE_DO_LOOP, line);
    n->data.do_loop.var = str_dup(var);
    n->data.do_loop.start = start;
    n->data.do_loop.end = end;
    n->data.do_loop.step = step;
    n->data.do_loop.body = body;
    n->data.do_loop.nbody = nbody;
    return n;
}

ASTNode *ast_do_while(ASTNode *cond, ASTNode **body, int nbody, int line) {
    ASTNode *n = alloc_node(NODE_DO_WHILE, line);
    n->data.do_while.cond = cond;
    n->data.do_while.body = body;
    n->data.do_while.nbody = nbody;
    return n;
}

ASTNode *ast_print(ASTNode **items, int nitems, int line) {
    ASTNode *n = alloc_node(NODE_PRINT, line);
    n->data.print.items = items;
    n->data.print.nitems = nitems;
    return n;
}

ASTNode *ast_read(char **vars, int nvars, int line) {
    ASTNode *n = alloc_node(NODE_READ, line);
    n->data.read.vars = vars;
    n->data.read.nvars = nvars;
    return n;
}

ASTNode *ast_call(const char *name, ASTNode **args, int nargs, int line) {
    ASTNode *n = alloc_node(NODE_CALL, line);
    n->data.call.name = str_dup(name);
    n->data.call.args = args;
    n->data.call.nargs = nargs;
    return n;
}

ASTNode *ast_return(int line) {
    return alloc_node(NODE_RETURN, line);
}

ASTNode *ast_stop(int line) {
    return alloc_node(NODE_STOP, line);
}

ASTNode *ast_exit(int line) {
    return alloc_node(NODE_EXIT, line);
}

ASTNode *ast_cycle(int line) {
    return alloc_node(NODE_CYCLE, line);
}

ASTNode *ast_decl(char **names, int nnames, IntentType intent, int line) {
    ASTNode *n = alloc_node(NODE_DECL, line);
    n->data.decl.names = names;
    n->data.decl.nnames = nnames;
    n->data.decl.intent = intent;
    return n;
}

ASTNode *ast_int_lit(int value, int line) {
    ASTNode *n = alloc_node(NODE_INT_LIT, line);
    n->data.int_lit.value = value;
    return n;
}

ASTNode *ast_str_lit(const char *value, int line) {
    ASTNode *n = alloc_node(NODE_STR_LIT, line);
    n->data.str_lit.value = str_dup(value);
    return n;
}

ASTNode *ast_var_ref(const char *name, int line) {
    ASTNode *n = alloc_node(NODE_VAR_REF, line);
    n->data.var_ref.name = str_dup(name);
    return n;
}

ASTNode *ast_binop(OpType op, ASTNode *left, ASTNode *right, int line) {
    ASTNode *n = alloc_node(NODE_BINOP, line);
    n->data.binop.op = op;
    n->data.binop.left = left;
    n->data.binop.right = right;
    return n;
}

ASTNode *ast_unary(OpType op, ASTNode *operand, int line) {
    ASTNode *n = alloc_node(NODE_UNARY, line);
    n->data.unary.op = op;
    n->data.unary.operand = operand;
    return n;
}

ASTNode *ast_func_call(const char *name, ASTNode **args, int nargs, int line) {
    ASTNode *n = alloc_node(NODE_FUNC_CALL, line);
    n->data.func_call.name = str_dup(name);
    n->data.func_call.args = args;
    n->data.func_call.nargs = nargs;
    return n;
}

ASTNode *ast_block(ASTNode **stmts, int nstmts, int line) {
    ASTNode *n = alloc_node(NODE_BLOCK, line);
    n->data.block.stmts = stmts;
    n->data.block.nstmts = nstmts;
    return n;
}

void ast_free(ASTNode *node) {
    if (!node) return;

    switch (node->type) {
        case NODE_PROGRAM:
            free(node->data.program.name);
            for (int i = 0; i < node->data.program.nstmts; i++)
                ast_free(node->data.program.stmts[i]);
            free(node->data.program.stmts);
            break;

        case NODE_FUNC_DEF:
            free(node->data.func_def.name);
            for (int i = 0; i < node->data.func_def.nparams; i++)
                free(node->data.func_def.params[i].name);
            free(node->data.func_def.params);
            for (int i = 0; i < node->data.func_def.nbody; i++)
                ast_free(node->data.func_def.body[i]);
            free(node->data.func_def.body);
            break;

        case NODE_SUB_DEF:
            free(node->data.sub_def.name);
            for (int i = 0; i < node->data.sub_def.nparams; i++)
                free(node->data.sub_def.params[i].name);
            free(node->data.sub_def.params);
            for (int i = 0; i < node->data.sub_def.nbody; i++)
                ast_free(node->data.sub_def.body[i]);
            free(node->data.sub_def.body);
            break;

        case NODE_ASSIGN:
            free(node->data.assign.target);
            ast_free(node->data.assign.value);
            break;

        case NODE_IF:
            ast_free(node->data.if_stmt.cond);
            for (int i = 0; i < node->data.if_stmt.nthen; i++)
                ast_free(node->data.if_stmt.then_body[i]);
            free(node->data.if_stmt.then_body);
            ast_free(node->data.if_stmt.else_branch);
            break;

        case NODE_DO_LOOP:
            free(node->data.do_loop.var);
            ast_free(node->data.do_loop.start);
            ast_free(node->data.do_loop.end);
            ast_free(node->data.do_loop.step);
            for (int i = 0; i < node->data.do_loop.nbody; i++)
                ast_free(node->data.do_loop.body[i]);
            free(node->data.do_loop.body);
            break;

        case NODE_DO_WHILE:
            ast_free(node->data.do_while.cond);
            for (int i = 0; i < node->data.do_while.nbody; i++)
                ast_free(node->data.do_while.body[i]);
            free(node->data.do_while.body);
            break;

        case NODE_PRINT:
            for (int i = 0; i < node->data.print.nitems; i++)
                ast_free(node->data.print.items[i]);
            free(node->data.print.items);
            break;

        case NODE_READ:
            for (int i = 0; i < node->data.read.nvars; i++)
                free(node->data.read.vars[i]);
            free(node->data.read.vars);
            break;

        case NODE_CALL:
            free(node->data.call.name);
            for (int i = 0; i < node->data.call.nargs; i++)
                ast_free(node->data.call.args[i]);
            free(node->data.call.args);
            break;

        case NODE_DECL:
            for (int i = 0; i < node->data.decl.nnames; i++)
                free(node->data.decl.names[i]);
            free(node->data.decl.names);
            break;

        case NODE_STR_LIT:
            free(node->data.str_lit.value);
            break;

        case NODE_VAR_REF:
            free(node->data.var_ref.name);
            break;

        case NODE_BINOP:
            ast_free(node->data.binop.left);
            ast_free(node->data.binop.right);
            break;

        case NODE_UNARY:
            ast_free(node->data.unary.operand);
            break;

        case NODE_FUNC_CALL:
            free(node->data.func_call.name);
            for (int i = 0; i < node->data.func_call.nargs; i++)
                ast_free(node->data.func_call.args[i]);
            free(node->data.func_call.args);
            break;

        case NODE_BLOCK:
            for (int i = 0; i < node->data.block.nstmts; i++)
                ast_free(node->data.block.stmts[i]);
            free(node->data.block.stmts);
            break;

        case NODE_INT_LIT:
        case NODE_RETURN:
        case NODE_STOP:
        case NODE_EXIT:
        case NODE_CYCLE:
            break;
    }

    free(node);
}
