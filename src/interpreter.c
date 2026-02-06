#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdarg.h>
#include <ctype.h>
#include <setjmp.h>
#include "interpreter.h"

/* ── Variable / Scope ─────────────────────────────────────────── */

typedef struct {
    char *name;
    int value;        /* scalar value */
    int *array_data;  /* NULL for scalars, heap-allocated for arrays */
    int array_size;   /* 0 for scalars, >0 for arrays */
} Variable;

typedef struct Scope {
    Variable *vars;
    int nvars;
    int capacity;
    struct Scope *parent;
} Scope;

static char *my_strdup(const char *s) {
    size_t len = strlen(s);
    char *d = malloc(len + 1);
    if (d) memcpy(d, s, len + 1);
    return d;
}

static Scope *scope_new(Scope *parent) {
    Scope *s = calloc(1, sizeof(Scope));
    s->parent = parent;
    return s;
}

static void scope_free(Scope *s) {
    for (int i = 0; i < s->nvars; i++) {
        free(s->vars[i].name);
        free(s->vars[i].array_data);
    }
    free(s->vars);
    free(s);
}

static Variable *scope_find_local(Scope *s, const char *name) {
    for (int i = 0; i < s->nvars; i++) {
        if (strcmp(s->vars[i].name, name) == 0)
            return &s->vars[i];
    }
    return NULL;
}

static Variable *scope_find(Scope *s, const char *name) {
    for (Scope *cur = s; cur; cur = cur->parent) {
        Variable *v = scope_find_local(cur, name);
        if (v) return v;
    }
    return NULL;
}

static void scope_set(Scope *s, const char *name, int value) {
    Variable *v = scope_find_local(s, name);
    if (v) {
        v->value = value;
        return;
    }
    /* Add to current scope */
    if (s->nvars >= s->capacity) {
        s->capacity = s->capacity == 0 ? 8 : s->capacity * 2;
        s->vars = realloc(s->vars, (size_t)s->capacity * sizeof(Variable));
    }
    s->vars[s->nvars].name = my_strdup(name);
    s->vars[s->nvars].value = value;
    s->vars[s->nvars].array_data = NULL;
    s->vars[s->nvars].array_size = 0;
    s->nvars++;
}

static void scope_declare(Scope *s, const char *name) {
    if (!scope_find_local(s, name)) {
        scope_set(s, name, 0);
    }
}

static void scope_declare_array(Scope *s, const char *name, int size) {
    if (scope_find_local(s, name)) return;
    if (s->nvars >= s->capacity) {
        s->capacity = s->capacity == 0 ? 8 : s->capacity * 2;
        s->vars = realloc(s->vars, (size_t)s->capacity * sizeof(Variable));
    }
    s->vars[s->nvars].name = my_strdup(name);
    s->vars[s->nvars].value = 0;
    s->vars[s->nvars].array_data = calloc((size_t)size, sizeof(int));
    s->vars[s->nvars].array_size = size;
    s->nvars++;
}

/* ── Function table ───────────────────────────────────────────── */

typedef struct {
    char *name;
    ASTNode *node;  /* NODE_FUNC_DEF or NODE_SUB_DEF */
} FuncEntry;

/* ── Control flow signals ─────────────────────────────────────── */

typedef enum {
    FLOW_NORMAL,
    FLOW_RETURN,
    FLOW_STOP,
    FLOW_EXIT,   /* break from DO loop */
    FLOW_CYCLE   /* continue in DO loop */
} FlowSignal;

/* ── Interpreter struct ───────────────────────────────────────── */

struct Interpreter {
    FuncEntry *funcs;
    int nfuncs;
    int func_cap;

    ASTNode *program;     /* the PROGRAM node */
    Scope *global_scope;
    Scope *current_scope;
    FlowSignal flow;

    jmp_buf stop_jmp;
    int stop_active;
};

/* ── Forward declarations ─────────────────────────────────────── */

static void exec_stmt(Interpreter *interp, ASTNode *node);
static int eval_expr(Interpreter *interp, ASTNode *node);
static void exec_body(Interpreter *interp, ASTNode **stmts, int nstmts);

/* ── Error handling ───────────────────────────────────────────── */

static void runtime_error(Interpreter *interp, int line, const char *msg) {
    fprintf(stderr, "Runtime error at line %d: %s\n", line, msg);
    if (interp->stop_active) {
        longjmp(interp->stop_jmp, 1);
    }
    exit(1);
}

static void runtime_errorf(Interpreter *interp, int line, const char *fmt, ...) {
    char buf[256];
    va_list ap;
    va_start(ap, fmt);
    vsnprintf(buf, sizeof(buf), fmt, ap);
    va_end(ap);
    runtime_error(interp, line, buf);
}

/* ── Function table operations ────────────────────────────────── */

static void register_func(Interpreter *interp, const char *name, ASTNode *node) {
    if (interp->nfuncs >= interp->func_cap) {
        interp->func_cap = interp->func_cap == 0 ? 8 : interp->func_cap * 2;
        interp->funcs = realloc(interp->funcs, (size_t)interp->func_cap * sizeof(FuncEntry));
    }
    interp->funcs[interp->nfuncs].name = my_strdup(name);
    interp->funcs[interp->nfuncs].node = node;
    interp->nfuncs++;
}

static ASTNode *find_func(Interpreter *interp, const char *name) {
    for (int i = 0; i < interp->nfuncs; i++) {
        if (strcmp(interp->funcs[i].name, name) == 0)
            return interp->funcs[i].node;
    }
    return NULL;
}

/* ── Intrinsic functions ──────────────────────────────────────── */

static int call_intrinsic(Interpreter *interp, const char *name,
                          ASTNode **args, int nargs, int line, int *result) {
    if (strcmp(name, "MOD") == 0) {
        if (nargs != 2) {
            runtime_errorf(interp, line, "MOD requires 2 arguments, got %d", nargs);
        }
        int a = eval_expr(interp, args[0]);
        int b = eval_expr(interp, args[1]);
        if (b == 0) runtime_error(interp, line, "Division by zero in MOD");
        *result = a % b;
        return 1;
    }
    if (strcmp(name, "ABS") == 0) {
        if (nargs != 1) {
            runtime_errorf(interp, line, "ABS requires 1 argument, got %d", nargs);
        }
        int a = eval_expr(interp, args[0]);
        *result = a < 0 ? -a : a;
        return 1;
    }
    return 0; /* not an intrinsic */
}

/* ── Expression evaluation ────────────────────────────────────── */

static int eval_expr(Interpreter *interp, ASTNode *node) {
    if (!node) return 0;

    switch (node->type) {
        case NODE_INT_LIT:
            return node->data.int_lit.value;

        case NODE_STR_LIT:
            runtime_error(interp, node->line,
                          "String used in integer expression");
            return 0;

        case NODE_VAR_REF: {
            Variable *v = scope_find(interp->current_scope, node->data.var_ref.name);
            if (!v) {
                runtime_errorf(interp, node->line,
                               "Undefined variable '%s'", node->data.var_ref.name);
            }
            return v->value;
        }

        case NODE_BINOP: {
            int left = eval_expr(interp, node->data.binop.left);
            int right = eval_expr(interp, node->data.binop.right);
            switch (node->data.binop.op) {
                case OP_ADD: return left + right;
                case OP_SUB: return left - right;
                case OP_MUL: return left * right;
                case OP_DIV:
                    if (right == 0) runtime_error(interp, node->line, "Division by zero");
                    return left / right;
                case OP_EQ:  return left == right ? 1 : 0;
                case OP_NE:  return left != right ? 1 : 0;
                case OP_LT:  return left < right  ? 1 : 0;
                case OP_GT:  return left > right  ? 1 : 0;
                case OP_LE:  return left <= right ? 1 : 0;
                case OP_GE:  return left >= right ? 1 : 0;
                case OP_AND: return (left && right) ? 1 : 0;
                case OP_OR:  return (left || right) ? 1 : 0;
                default:
                    runtime_error(interp, node->line, "Unknown binary operator");
                    return 0;
            }
        }

        case NODE_UNARY: {
            int operand = eval_expr(interp, node->data.unary.operand);
            switch (node->data.unary.op) {
                case OP_NEG: return -operand;
                case OP_NOT: return operand ? 0 : 1;
                default:
                    runtime_error(interp, node->line, "Unknown unary operator");
                    return 0;
            }
        }

        case NODE_FUNC_CALL: {
            const char *name = node->data.func_call.name;
            ASTNode **args = node->data.func_call.args;
            int nargs = node->data.func_call.nargs;

            /* Check if this is an array element access */
            {
                Variable *av = scope_find(interp->current_scope, name);
                if (av && av->array_data) {
                    if (nargs != 1) {
                        runtime_errorf(interp, node->line,
                                       "Array '%s' requires exactly 1 index", name);
                    }
                    int idx = eval_expr(interp, args[0]);
                    if (idx < 1 || idx > av->array_size) {
                        runtime_errorf(interp, node->line,
                                       "Array index %d out of bounds for '%s' (1..%d)",
                                       idx, name, av->array_size);
                    }
                    return av->array_data[idx - 1];
                }
            }

            /* Try intrinsics first */
            int result;
            if (call_intrinsic(interp, name, args, nargs, node->line, &result)) {
                return result;
            }

            /* Look up user function */
            ASTNode *fdef = find_func(interp, name);
            if (!fdef || fdef->type != NODE_FUNC_DEF) {
                runtime_errorf(interp, node->line, "Undefined function '%s'", name);
            }

            int nparams = fdef->data.func_def.nparams;
            if (nargs != nparams) {
                runtime_errorf(interp, node->line,
                               "Function '%s' expects %d arguments, got %d",
                               name, nparams, nargs);
            }

            /* Evaluate arguments */
            int *arg_vals = malloc((size_t)nargs * sizeof(int));
            for (int i = 0; i < nargs; i++) {
                arg_vals[i] = eval_expr(interp, args[i]);
            }

            /* Push new scope */
            Scope *caller_scope = interp->current_scope;
            Scope *func_scope = scope_new(interp->global_scope);
            interp->current_scope = func_scope;

            /* Bind parameters */
            for (int i = 0; i < nparams; i++) {
                scope_set(func_scope, fdef->data.func_def.params[i].name, arg_vals[i]);
            }
            free(arg_vals);

            /* Initialize function name variable (for return value) */
            scope_declare(func_scope, name);

            /* Execute body */
            FlowSignal saved_flow = interp->flow;
            interp->flow = FLOW_NORMAL;
            exec_body(interp, fdef->data.func_def.body, fdef->data.func_def.nbody);
            interp->flow = saved_flow;

            /* Get return value (function name = result) */
            Variable *rv = scope_find_local(func_scope, name);
            result = rv ? rv->value : 0;

            /* Pop scope */
            interp->current_scope = caller_scope;
            scope_free(func_scope);

            return result;
        }

        default:
            runtime_errorf(interp, node->line, "Invalid expression node type %d", node->type);
            return 0;
    }
}

/* ── Statement execution ──────────────────────────────────────── */

static void exec_body(Interpreter *interp, ASTNode **stmts, int nstmts) {
    for (int i = 0; i < nstmts; i++) {
        if (interp->flow != FLOW_NORMAL) break;
        exec_stmt(interp, stmts[i]);
    }
}

static void exec_stmt(Interpreter *interp, ASTNode *node) {
    if (!node || interp->flow != FLOW_NORMAL) return;

    switch (node->type) {
        case NODE_DECL: {
            for (int i = 0; i < node->data.decl.nnames; i++) {
                int sz = node->data.decl.sizes[i];
                if (sz > 0) {
                    scope_declare_array(interp->current_scope, node->data.decl.names[i], sz);
                } else {
                    scope_declare(interp->current_scope, node->data.decl.names[i]);
                }
            }
            break;
        }

        case NODE_ASSIGN: {
            int value = eval_expr(interp, node->data.assign.value);
            if (node->data.assign.index) {
                /* Array element assignment: ARR(idx) = value */
                int idx = eval_expr(interp, node->data.assign.index);
                Variable *v = scope_find(interp->current_scope, node->data.assign.target);
                if (!v || !v->array_data) {
                    runtime_errorf(interp, node->line,
                                   "'%s' is not an array", node->data.assign.target);
                }
                if (idx < 1 || idx > v->array_size) {
                    runtime_errorf(interp, node->line,
                                   "Array index %d out of bounds for '%s' (1..%d)",
                                   idx, node->data.assign.target, v->array_size);
                }
                v->array_data[idx - 1] = value;
            } else {
                /* Scalar assignment */
                Variable *v = scope_find(interp->current_scope, node->data.assign.target);
                if (v) {
                    v->value = value;
                } else {
                    scope_set(interp->current_scope, node->data.assign.target, value);
                }
            }
            break;
        }

        case NODE_IF: {
            int cond = eval_expr(interp, node->data.if_stmt.cond);
            if (cond) {
                exec_body(interp, node->data.if_stmt.then_body, node->data.if_stmt.nthen);
            } else if (node->data.if_stmt.else_branch) {
                ASTNode *eb = node->data.if_stmt.else_branch;
                if (eb->type == NODE_IF) {
                    exec_stmt(interp, eb);
                } else if (eb->type == NODE_BLOCK) {
                    exec_body(interp, eb->data.block.stmts, eb->data.block.nstmts);
                }
            }
            break;
        }

        case NODE_DO_LOOP: {
            int start = eval_expr(interp, node->data.do_loop.start);
            int end = eval_expr(interp, node->data.do_loop.end);
            int step = node->data.do_loop.step ? eval_expr(interp, node->data.do_loop.step) : 1;

            if (step == 0) {
                runtime_error(interp, node->line, "DO loop step cannot be zero");
            }

            /* Ensure loop variable exists */
            Variable *v = scope_find(interp->current_scope, node->data.do_loop.var);
            if (!v) {
                scope_set(interp->current_scope, node->data.do_loop.var, start);
                v = scope_find(interp->current_scope, node->data.do_loop.var);
            } else {
                v->value = start;
            }

            while ((step > 0 && v->value <= end) || (step < 0 && v->value >= end)) {
                exec_body(interp, node->data.do_loop.body, node->data.do_loop.nbody);

                if (interp->flow == FLOW_EXIT) {
                    interp->flow = FLOW_NORMAL;
                    break;
                }
                if (interp->flow == FLOW_CYCLE) {
                    interp->flow = FLOW_NORMAL;
                }
                if (interp->flow != FLOW_NORMAL) break;

                v->value += step;
            }
            break;
        }

        case NODE_DO_WHILE: {
            while (interp->flow == FLOW_NORMAL) {
                int cond = eval_expr(interp, node->data.do_while.cond);
                if (!cond) break;

                exec_body(interp, node->data.do_while.body, node->data.do_while.nbody);

                if (interp->flow == FLOW_EXIT) {
                    interp->flow = FLOW_NORMAL;
                    break;
                }
                if (interp->flow == FLOW_CYCLE) {
                    interp->flow = FLOW_NORMAL;
                }
            }
            break;
        }

        case NODE_PRINT: {
            for (int i = 0; i < node->data.print.nitems; i++) {
                ASTNode *item = node->data.print.items[i];
                if (item->type == NODE_STR_LIT) {
                    printf("%s", item->data.str_lit.value);
                } else {
                    int val = eval_expr(interp, item);
                    if (i > 0) printf(" ");
                    printf("%d", val);
                }
            }
            printf("\n");
            break;
        }

        case NODE_READ: {
            for (int i = 0; i < node->data.read.nvars; i++) {
                int val;
                if (scanf("%d", &val) != 1) {
                    runtime_error(interp, node->line, "Failed to read integer from input");
                }
                ASTNode *idx_node = node->data.read.indices ? node->data.read.indices[i] : NULL;
                if (idx_node) {
                    /* Array element: READ *, ARR(expr) */
                    int idx = eval_expr(interp, idx_node);
                    Variable *v = scope_find(interp->current_scope, node->data.read.vars[i]);
                    if (!v || !v->array_data) {
                        runtime_errorf(interp, node->line,
                                       "'%s' is not an array", node->data.read.vars[i]);
                    }
                    if (idx < 1 || idx > v->array_size) {
                        runtime_errorf(interp, node->line,
                                       "Array index %d out of bounds for '%s' (1..%d)",
                                       idx, node->data.read.vars[i], v->array_size);
                    }
                    v->array_data[idx - 1] = val;
                } else {
                    /* Scalar */
                    Variable *v = scope_find(interp->current_scope, node->data.read.vars[i]);
                    if (v) {
                        v->value = val;
                    } else {
                        scope_set(interp->current_scope, node->data.read.vars[i], val);
                    }
                }
            }
            break;
        }

        case NODE_CALL: {
            const char *name = node->data.call.name;
            ASTNode **args = node->data.call.args;
            int nargs = node->data.call.nargs;

            ASTNode *sdef = find_func(interp, name);
            if (!sdef || sdef->type != NODE_SUB_DEF) {
                runtime_errorf(interp, node->line, "Undefined subroutine '%s'", name);
            }

            int nparams = sdef->data.sub_def.nparams;
            if (nargs != nparams) {
                runtime_errorf(interp, node->line,
                               "Subroutine '%s' expects %d arguments, got %d",
                               name, nparams, nargs);
            }

            /* Evaluate arguments and track writeback targets */
            int *arg_vals = malloc((size_t)nargs * sizeof(int));
            char **arg_var_names = calloc((size_t)nargs, sizeof(char*));
            int *arg_arr_indices = calloc((size_t)nargs, sizeof(int)); /* 0 = not array */
            for (int i = 0; i < nargs; i++) {
                arg_vals[i] = eval_expr(interp, args[i]);
                if (args[i]->type == NODE_VAR_REF) {
                    arg_var_names[i] = args[i]->data.var_ref.name;
                } else if (args[i]->type == NODE_FUNC_CALL && args[i]->data.func_call.nargs == 1) {
                    /* Could be an array element: ARR(expr) */
                    Variable *av = scope_find(interp->current_scope, args[i]->data.func_call.name);
                    if (av && av->array_data) {
                        arg_var_names[i] = args[i]->data.func_call.name;
                        int idx = eval_expr(interp, args[i]->data.func_call.args[0]);
                        arg_arr_indices[i] = idx; /* 1-based index, 0 means not array */
                    }
                }
            }

            /* Push new scope */
            Scope *caller_scope = interp->current_scope;
            Scope *sub_scope = scope_new(interp->global_scope);
            interp->current_scope = sub_scope;

            /* Bind parameters */
            for (int i = 0; i < nparams; i++) {
                scope_set(sub_scope, sdef->data.sub_def.params[i].name, arg_vals[i]);
            }

            /* Execute body */
            FlowSignal saved_flow = interp->flow;
            interp->flow = FLOW_NORMAL;
            exec_body(interp, sdef->data.sub_def.body, sdef->data.sub_def.nbody);
            if (interp->flow == FLOW_RETURN) {
                interp->flow = FLOW_NORMAL;
            }
            if (interp->flow == FLOW_STOP) {
                /* Propagate STOP */
            } else {
                interp->flow = saved_flow;
            }

            /* Write back INOUT parameters */
            for (int i = 0; i < nparams; i++) {
                if (sdef->data.sub_def.params[i].intent == INTENT_INOUT) {
                    Variable *pv = scope_find_local(sub_scope, sdef->data.sub_def.params[i].name);
                    if (pv && arg_var_names[i]) {
                        Variable *cv = scope_find(caller_scope, arg_var_names[i]);
                        if (cv) {
                            if (arg_arr_indices[i] > 0 && cv->array_data) {
                                /* Write back to array element */
                                int idx = arg_arr_indices[i];
                                if (idx >= 1 && idx <= cv->array_size) {
                                    cv->array_data[idx - 1] = pv->value;
                                }
                            } else {
                                cv->value = pv->value;
                            }
                        }
                    }
                }
            }

            /* Pop scope */
            interp->current_scope = caller_scope;
            scope_free(sub_scope);
            free(arg_vals);
            free(arg_var_names);
            free(arg_arr_indices);
            break;
        }

        case NODE_RETURN:
            interp->flow = FLOW_RETURN;
            break;

        case NODE_STOP:
            interp->flow = FLOW_STOP;
            if (interp->stop_active) {
                longjmp(interp->stop_jmp, 1);
            }
            break;

        case NODE_EXIT:
            interp->flow = FLOW_EXIT;
            break;

        case NODE_CYCLE:
            interp->flow = FLOW_CYCLE;
            break;

        default:
            runtime_errorf(interp, node->line, "Unknown statement type %d", node->type);
            break;
    }
}

/* ── Public API ───────────────────────────────────────────────── */

Interpreter *interp_new(void) {
    Interpreter *interp = calloc(1, sizeof(Interpreter));
    interp->global_scope = scope_new(NULL);
    interp->current_scope = interp->global_scope;
    interp->flow = FLOW_NORMAL;
    interp->stop_active = 0;
    return interp;
}

void interp_free(Interpreter *interp) {
    for (int i = 0; i < interp->nfuncs; i++) {
        free(interp->funcs[i].name);
    }
    free(interp->funcs);
    scope_free(interp->global_scope);
    free(interp);
}

void interp_register(Interpreter *interp, ASTNode **units, int nunits) {
    for (int i = 0; i < nunits; i++) {
        ASTNode *u = units[i];
        switch (u->type) {
            case NODE_PROGRAM:
                interp->program = u;
                break;
            case NODE_FUNC_DEF:
                register_func(interp, u->data.func_def.name, u);
                break;
            case NODE_SUB_DEF:
                register_func(interp, u->data.sub_def.name, u);
                break;
            default:
                break;
        }
    }
}

int interp_run(Interpreter *interp) {
    if (!interp->program) {
        fprintf(stderr, "Error: No PROGRAM block found\n");
        return 1;
    }

    interp->stop_active = 1;
    if (setjmp(interp->stop_jmp) == 0) {
        exec_body(interp,
                  interp->program->data.program.stmts,
                  interp->program->data.program.nstmts);
    }
    /* STOP was called (or normal completion) */
    interp->stop_active = 0;

    return 0;
}
