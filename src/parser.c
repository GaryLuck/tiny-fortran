#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdarg.h>
#include <ctype.h>
#include "parser.h"
#include "lexer.h"

/* ── Parser state ─────────────────────────────────────────────── */

typedef struct {
    Lexer lex;
    Token cur;      /* current token */
    Token prev;     /* previous token (for line info) */
    int has_error;
    char error_msg[256];
    int error_line;
} Parser;

static char *my_strdup(const char *s) {
    size_t len = strlen(s);
    char *d = malloc(len + 1);
    if (d) memcpy(d, s, len + 1);
    return d;
}

static void str_upper(char *s) {
    for (; *s; s++) *s = (char)toupper((unsigned char)*s);
}

static void parser_init(Parser *p, const char *source) {
    lexer_init(&p->lex, source);
    p->cur = lexer_next(&p->lex);
    p->prev.type = TOK_EOF;
    p->prev.text = NULL;
    p->has_error = 0;
    p->error_msg[0] = '\0';
    p->error_line = 0;
}

static void parser_error(Parser *p, const char *msg) {
    if (!p->has_error) {
        p->has_error = 1;
        snprintf(p->error_msg, sizeof(p->error_msg), "%s", msg);
        p->error_line = p->cur.line;
    }
}

static void parser_errorf(Parser *p, const char *fmt, ...) {
    if (!p->has_error) {
        va_list ap;
        p->has_error = 1;
        va_start(ap, fmt);
        vsnprintf(p->error_msg, sizeof(p->error_msg), fmt, ap);
        va_end(ap);
        p->error_line = p->cur.line;
    }
}

static void next(Parser *p) {
    token_free(&p->prev);
    p->prev = p->cur;
    p->cur = lexer_next(&p->lex);
}

static int check(Parser *p, TokenType t) {
    return p->cur.type == t;
}

static int match(Parser *p, TokenType t) {
    if (p->cur.type == t) {
        next(p);
        return 1;
    }
    return 0;
}

static void expect(Parser *p, TokenType t) {
    if (p->cur.type == t) {
        next(p);
    } else {
        char msg[128];
        snprintf(msg, sizeof(msg), "Expected %s, got %s '%s'",
                 token_type_name(t), token_type_name(p->cur.type),
                 p->cur.text ? p->cur.text : "");
        parser_error(p, msg);
    }
}

static void skip_newlines(Parser *p) {
    while (p->cur.type == TOK_NEWLINE) {
        next(p);
    }
}

/* ── Forward declarations ─────────────────────────────────────── */

static ASTNode *parse_expr(Parser *p);
static ASTNode *parse_statement(Parser *p);
static ASTNode **parse_body(Parser *p, int *nstmts);

/* ── Dynamic array helper ─────────────────────────────────────── */

typedef struct {
    void **items;
    int count;
    int capacity;
} DynArray;

static void da_init(DynArray *da) {
    da->items = NULL;
    da->count = 0;
    da->capacity = 0;
}

static void da_push(DynArray *da, void *item) {
    if (da->count >= da->capacity) {
        da->capacity = da->capacity == 0 ? 8 : da->capacity * 2;
        da->items = realloc(da->items, (size_t)da->capacity * sizeof(void*));
    }
    da->items[da->count++] = item;
}

/* ── Expression parsing (precedence climbing) ─────────────────── */

static ASTNode *parse_primary(Parser *p);
static ASTNode *parse_unary(Parser *p);
static ASTNode *parse_expr_prec(Parser *p, int min_prec);

static int get_prec(TokenType t) {
    switch (t) {
        case TOK_OR:    return 1;
        case TOK_AND:   return 2;
        case TOK_EQ: case TOK_NE: case TOK_LT: case TOK_GT:
        case TOK_LE: case TOK_GE:
            return 3;
        case TOK_PLUS: case TOK_MINUS:
            return 4;
        case TOK_STAR: case TOK_SLASH:
            return 5;
        default:
            return -1;
    }
}

static OpType tok_to_op(TokenType t) {
    switch (t) {
        case TOK_PLUS:  return OP_ADD;
        case TOK_MINUS: return OP_SUB;
        case TOK_STAR:  return OP_MUL;
        case TOK_SLASH: return OP_DIV;
        case TOK_EQ:    return OP_EQ;
        case TOK_NE:    return OP_NE;
        case TOK_LT:    return OP_LT;
        case TOK_GT:    return OP_GT;
        case TOK_LE:    return OP_LE;
        case TOK_GE:    return OP_GE;
        case TOK_AND:   return OP_AND;
        case TOK_OR:    return OP_OR;
        default:        return OP_ADD; /* should not happen */
    }
}

static ASTNode *parse_primary(Parser *p) {
    int line = p->cur.line;

    if (p->has_error) return ast_int_lit(0, line);

    /* Integer literal */
    if (check(p, TOK_INT_LIT)) {
        int val = p->cur.int_value;
        next(p);
        return ast_int_lit(val, line);
    }

    /* String literal */
    if (check(p, TOK_STR_LIT)) {
        char *val = my_strdup(p->cur.text);
        next(p);
        ASTNode *n = ast_str_lit(val, line);
        free(val);
        return n;
    }

    /* Parenthesized expression */
    if (check(p, TOK_LPAREN)) {
        next(p);
        ASTNode *e = parse_expr(p);
        expect(p, TOK_RPAREN);
        return e;
    }

    /* Identifier: could be variable ref or function call */
    if (check(p, TOK_IDENT)) {
        char *name = my_strdup(p->cur.text);
        str_upper(name);
        next(p);

        /* Function call: IDENT ( args ) */
        if (check(p, TOK_LPAREN)) {
            next(p); /* consume '(' */
            DynArray args;
            da_init(&args);
            if (!check(p, TOK_RPAREN)) {
                da_push(&args, parse_expr(p));
                while (match(p, TOK_COMMA)) {
                    da_push(&args, parse_expr(p));
                }
            }
            expect(p, TOK_RPAREN);
            ASTNode *n = ast_func_call(name, (ASTNode**)args.items, args.count, line);
            free(name);
            return n;
        }

        ASTNode *n = ast_var_ref(name, line);
        free(name);
        return n;
    }

    parser_errorf(p, "Expected expression, got %s '%s'",
                  token_type_name(p->cur.type),
                  p->cur.text ? p->cur.text : "");
    return ast_int_lit(0, line);
}

static ASTNode *parse_unary(Parser *p) {
    int line = p->cur.line;

    if (check(p, TOK_MINUS)) {
        next(p);
        ASTNode *operand = parse_unary(p);
        return ast_unary(OP_NEG, operand, line);
    }
    if (check(p, TOK_NOT)) {
        next(p);
        ASTNode *operand = parse_unary(p);
        return ast_unary(OP_NOT, operand, line);
    }
    /* Unary plus: just ignore it */
    if (check(p, TOK_PLUS)) {
        next(p);
        return parse_unary(p);
    }

    return parse_primary(p);
}

static ASTNode *parse_expr_prec(Parser *p, int min_prec) {
    ASTNode *left = parse_unary(p);

    while (!p->has_error) {
        int prec = get_prec(p->cur.type);
        if (prec < min_prec) break;

        TokenType op_tok = p->cur.type;
        int line = p->cur.line;
        next(p);

        ASTNode *right = parse_expr_prec(p, prec + 1);
        left = ast_binop(tok_to_op(op_tok), left, right, line);
    }

    return left;
}

static ASTNode *parse_expr(Parser *p) {
    return parse_expr_prec(p, 1);
}

/* ── Statement parsing ────────────────────────────────────────── */

/* Check if current position is at "END PROGRAM", "END FUNCTION", etc. */
static int at_end_block(Parser *p) {
    if (check(p, TOK_END)) return 1;
    if (check(p, TOK_ELSE) || check(p, TOK_ELSEIF)) return 1;
    if (check(p, TOK_EOF)) return 1;
    return 0;
}

/* Parse body statements until END/ELSE/ELSEIF/EOF */
static ASTNode **parse_body(Parser *p, int *nstmts) {
    DynArray stmts;
    da_init(&stmts);

    skip_newlines(p);

    while (!p->has_error && !at_end_block(p)) {
        /* Check for "ELSE IF" (two tokens) which acts like ELSEIF */
        if (check(p, TOK_ELSE)) {
            /* Peek: is next meaningful token IF? handled by caller */
            break;
        }

        ASTNode *s = parse_statement(p);
        if (s) da_push(&stmts, s);
        skip_newlines(p);
    }

    *nstmts = stmts.count;
    return (ASTNode**)stmts.items;
}

static ASTNode *parse_if_stmt(Parser *p) {
    int line = p->cur.line;
    /* IF already consumed */
    expect(p, TOK_LPAREN);
    ASTNode *cond = parse_expr(p);
    expect(p, TOK_RPAREN);
    expect(p, TOK_THEN);
    skip_newlines(p);

    int nthen;
    ASTNode **then_body = parse_body(p, &nthen);

    ASTNode *else_branch = NULL;

    /* ELSE IF or ELSEIF */
    if (check(p, TOK_ELSEIF)) {
        next(p); /* consume ELSEIF */
        else_branch = parse_if_stmt(p);
        return ast_if(cond, then_body, nthen, else_branch, line);
    }

    if (check(p, TOK_ELSE)) {
        next(p); /* consume ELSE */
        /* Check for "ELSE IF" (two-word form) */
        skip_newlines(p);
        if (check(p, TOK_IF)) {
            next(p); /* consume IF */
            else_branch = parse_if_stmt(p);
            return ast_if(cond, then_body, nthen, else_branch, line);
        }

        /* Plain ELSE block */
        int nelse;
        ASTNode **else_body = parse_body(p, &nelse);
        else_branch = ast_block(else_body, nelse, p->cur.line);
    }

    /* END IF */
    expect(p, TOK_END);
    if (check(p, TOK_IF)) next(p); /* consume optional IF after END */

    return ast_if(cond, then_body, nthen, else_branch, line);
}

static ASTNode *parse_do_stmt(Parser *p) {
    int line = p->cur.line;
    /* DO already consumed */

    /* DO WHILE (cond) */
    if (check(p, TOK_WHILE)) {
        next(p);
        expect(p, TOK_LPAREN);
        ASTNode *cond = parse_expr(p);
        expect(p, TOK_RPAREN);
        skip_newlines(p);

        int nbody;
        ASTNode **body = parse_body(p, &nbody);
        expect(p, TOK_END);
        if (check(p, TOK_DO)) next(p);
        return ast_do_while(cond, body, nbody, line);
    }

    /* DO var = start, end [, step] */
    if (!check(p, TOK_IDENT)) {
        parser_error(p, "Expected loop variable after DO");
        return ast_int_lit(0, line);
    }

    char *var = my_strdup(p->cur.text);
    str_upper(var);
    next(p);
    expect(p, TOK_ASSIGN);
    ASTNode *start = parse_expr(p);
    expect(p, TOK_COMMA);
    ASTNode *end = parse_expr(p);

    ASTNode *step = NULL;
    if (match(p, TOK_COMMA)) {
        step = parse_expr(p);
    }
    skip_newlines(p);

    int nbody;
    ASTNode **body = parse_body(p, &nbody);
    expect(p, TOK_END);
    if (check(p, TOK_DO)) next(p);

    ASTNode *n = ast_do_loop(var, start, end, step, body, nbody, line);
    free(var);
    return n;
}

static ASTNode *parse_print_stmt(Parser *p) {
    int line = p->cur.line;
    /* PRINT already consumed */
    expect(p, TOK_STAR);
    /* optional comma after * */
    match(p, TOK_COMMA);

    DynArray items;
    da_init(&items);

    /* Parse items until newline/EOF */
    if (!check(p, TOK_NEWLINE) && !check(p, TOK_EOF)) {
        da_push(&items, parse_expr(p));
        while (match(p, TOK_COMMA)) {
            da_push(&items, parse_expr(p));
        }
    }

    return ast_print((ASTNode**)items.items, items.count, line);
}

static ASTNode *parse_read_stmt(Parser *p) {
    int line = p->cur.line;
    /* READ already consumed */
    expect(p, TOK_STAR);
    match(p, TOK_COMMA);

    DynArray vars;
    da_init(&vars);

    if (check(p, TOK_IDENT)) {
        char *name = my_strdup(p->cur.text);
        str_upper(name);
        da_push(&vars, name);
        next(p);
        while (match(p, TOK_COMMA)) {
            if (check(p, TOK_IDENT)) {
                name = my_strdup(p->cur.text);
                str_upper(name);
                da_push(&vars, name);
                next(p);
            }
        }
    }

    return ast_read((char**)vars.items, vars.count, line);
}

static ASTNode *parse_call_stmt(Parser *p) {
    int line = p->cur.line;
    /* CALL already consumed */
    if (!check(p, TOK_IDENT)) {
        parser_error(p, "Expected subroutine name after CALL");
        return ast_int_lit(0, line);
    }
    char *name = my_strdup(p->cur.text);
    str_upper(name);
    next(p);

    DynArray args;
    da_init(&args);

    if (match(p, TOK_LPAREN)) {
        if (!check(p, TOK_RPAREN)) {
            da_push(&args, parse_expr(p));
            while (match(p, TOK_COMMA)) {
                da_push(&args, parse_expr(p));
            }
        }
        expect(p, TOK_RPAREN);
    }

    ASTNode *n = ast_call(name, (ASTNode**)args.items, args.count, line);
    free(name);
    return n;
}

static ASTNode *parse_decl(Parser *p) {
    int line = p->cur.line;
    /* INTEGER already consumed */

    IntentType intent = INTENT_NONE;

    /* Optional: , INTENT(IN) or , INTENT(INOUT) */
    if (check(p, TOK_COMMA)) {
        next(p);
        if (check(p, TOK_INTENT)) {
            next(p);
            expect(p, TOK_LPAREN);
            if (check(p, TOK_INOUT)) {
                intent = INTENT_INOUT;
                next(p);
            } else if (check(p, TOK_IN)) {
                intent = INTENT_IN;
                next(p);
            } else {
                parser_error(p, "Expected IN or INOUT");
            }
            expect(p, TOK_RPAREN);
        } else {
            parser_error(p, "Expected INTENT after comma in declaration");
        }
    }

    expect(p, TOK_DCOLON);

    DynArray names;
    da_init(&names);

    if (check(p, TOK_IDENT)) {
        char *name = my_strdup(p->cur.text);
        str_upper(name);
        da_push(&names, name);
        next(p);
        while (match(p, TOK_COMMA)) {
            if (check(p, TOK_IDENT)) {
                name = my_strdup(p->cur.text);
                str_upper(name);
                da_push(&names, name);
                next(p);
            }
        }
    }

    return ast_decl((char**)names.items, names.count, intent, line);
}

static ASTNode *parse_statement(Parser *p) {
    skip_newlines(p);

    if (p->has_error || check(p, TOK_EOF)) return NULL;

    int line = p->cur.line;

    if (check(p, TOK_IF)) {
        next(p);
        return parse_if_stmt(p);
    }

    if (check(p, TOK_DO)) {
        next(p);
        return parse_do_stmt(p);
    }

    if (check(p, TOK_PRINT)) {
        next(p);
        return parse_print_stmt(p);
    }

    if (check(p, TOK_READ)) {
        next(p);
        return parse_read_stmt(p);
    }

    if (check(p, TOK_CALL)) {
        next(p);
        return parse_call_stmt(p);
    }

    if (check(p, TOK_RETURN)) {
        next(p);
        return ast_return(line);
    }

    if (check(p, TOK_STOP)) {
        next(p);
        return ast_stop(line);
    }

    if (check(p, TOK_EXIT)) {
        next(p);
        return ast_exit(line);
    }

    if (check(p, TOK_CYCLE)) {
        next(p);
        return ast_cycle(line);
    }

    if (check(p, TOK_INTEGER)) {
        next(p);
        return parse_decl(p);
    }

    /* Assignment: IDENT = expr */
    if (check(p, TOK_IDENT)) {
        char *name = my_strdup(p->cur.text);
        str_upper(name);
        next(p);

        if (match(p, TOK_ASSIGN)) {
            ASTNode *value = parse_expr(p);
            ASTNode *n = ast_assign(name, value, line);
            free(name);
            return n;
        }

        /* Not an assignment — could be a function call used as statement (invalid in Fortran
           but let's give a decent error). Roll back is complex, so just error. */
        parser_errorf(p, "Expected '=' after '%s' for assignment", name);
        free(name);
        return NULL;
    }

    parser_errorf(p, "Unexpected token %s '%s'",
                  token_type_name(p->cur.type),
                  p->cur.text ? p->cur.text : "");
    next(p); /* skip to avoid infinite loop */
    return NULL;
}

/* ── Top-level parsing ────────────────────────────────────────── */

static ParamDef *parse_params(Parser *p, int *nparams) {
    DynArray params;
    da_init(&params);

    if (match(p, TOK_LPAREN)) {
        if (!check(p, TOK_RPAREN)) {
            if (check(p, TOK_IDENT)) {
                ParamDef *pd = malloc(sizeof(ParamDef));
                pd->name = my_strdup(p->cur.text);
                str_upper(pd->name);
                pd->intent = INTENT_NONE;
                da_push(&params, pd);
                next(p);
            }
            while (match(p, TOK_COMMA)) {
                if (check(p, TOK_IDENT)) {
                    ParamDef *pd = malloc(sizeof(ParamDef));
                    pd->name = my_strdup(p->cur.text);
                    str_upper(pd->name);
                    pd->intent = INTENT_NONE;
                    da_push(&params, pd);
                    next(p);
                }
            }
        }
        expect(p, TOK_RPAREN);
    }

    /* Convert pointer array to ParamDef array */
    *nparams = params.count;
    if (params.count == 0) {
        free(params.items);
        return NULL;
    }
    ParamDef *result = malloc((size_t)params.count * sizeof(ParamDef));
    for (int i = 0; i < params.count; i++) {
        ParamDef *pd = (ParamDef*)params.items[i];
        result[i] = *pd;
        free(pd);
    }
    free(params.items);
    return result;
}

/* Update param intents from INTENT declarations in body */
static void update_param_intents(ParamDef *params, int nparams,
                                 ASTNode **body, int nbody) {
    for (int i = 0; i < nbody; i++) {
        if (body[i] && body[i]->type == NODE_DECL && body[i]->data.decl.intent != INTENT_NONE) {
            for (int j = 0; j < body[i]->data.decl.nnames; j++) {
                for (int k = 0; k < nparams; k++) {
                    if (strcmp(params[k].name, body[i]->data.decl.names[j]) == 0) {
                        params[k].intent = body[i]->data.decl.intent;
                    }
                }
            }
        }
    }
}

static ASTNode *parse_program(Parser *p) {
    int line = p->cur.line;
    expect(p, TOK_PROGRAM);
    char *name = my_strdup(p->cur.text);
    str_upper(name);
    next(p);
    skip_newlines(p);

    int nstmts;
    ASTNode **stmts = parse_body(p, &nstmts);

    /* END PROGRAM [name] */
    expect(p, TOK_END);
    if (check(p, TOK_PROGRAM)) {
        next(p);
        if (check(p, TOK_IDENT)) next(p); /* optional name */
    }

    ASTNode *n = ast_program(name, stmts, nstmts, line);
    free(name);
    return n;
}

static ASTNode *parse_function(Parser *p) {
    int line = p->cur.line;
    /* INTEGER FUNCTION already consumed, or just FUNCTION */

    if (!check(p, TOK_IDENT)) {
        parser_error(p, "Expected function name");
        return ast_int_lit(0, line);
    }
    char *name = my_strdup(p->cur.text);
    str_upper(name);
    next(p);

    int nparams;
    ParamDef *params = parse_params(p, &nparams);
    skip_newlines(p);

    int nbody;
    ASTNode **body = parse_body(p, &nbody);

    update_param_intents(params, nparams, body, nbody);

    /* END FUNCTION [name] */
    expect(p, TOK_END);
    if (check(p, TOK_FUNCTION)) {
        next(p);
        if (check(p, TOK_IDENT)) next(p);
    }

    ASTNode *n = ast_func_def(name, params, nparams, body, nbody, line);
    free(name);
    return n;
}

static ASTNode *parse_subroutine(Parser *p) {
    int line = p->cur.line;
    /* SUBROUTINE already consumed */

    if (!check(p, TOK_IDENT)) {
        parser_error(p, "Expected subroutine name");
        return ast_int_lit(0, line);
    }
    char *name = my_strdup(p->cur.text);
    str_upper(name);
    next(p);

    int nparams;
    ParamDef *params = parse_params(p, &nparams);
    skip_newlines(p);

    int nbody;
    ASTNode **body = parse_body(p, &nbody);

    update_param_intents(params, nparams, body, nbody);

    /* END SUBROUTINE [name] */
    expect(p, TOK_END);
    if (check(p, TOK_SUBROUTINE)) {
        next(p);
        if (check(p, TOK_IDENT)) next(p);
    }

    ASTNode *n = ast_sub_def(name, params, nparams, body, nbody, line);
    free(name);
    return n;
}

/* ── Public API ───────────────────────────────────────────────── */

ParseResult parse(const char *source) {
    Parser p;
    parser_init(&p, source);

    DynArray units;
    da_init(&units);

    skip_newlines(&p);

    while (!p.has_error && p.cur.type != TOK_EOF) {
        if (check(&p, TOK_PROGRAM)) {
            da_push(&units, parse_program(&p));
        } else if (check(&p, TOK_INTEGER)) {
            next(&p);
            if (check(&p, TOK_FUNCTION)) {
                next(&p);
                da_push(&units, parse_function(&p));
            } else {
                parser_error(&p, "Expected FUNCTION after INTEGER at top level");
            }
        } else if (check(&p, TOK_FUNCTION)) {
            next(&p);
            da_push(&units, parse_function(&p));
        } else if (check(&p, TOK_SUBROUTINE)) {
            next(&p);
            da_push(&units, parse_subroutine(&p));
        } else {
            parser_errorf(&p, "Expected PROGRAM, FUNCTION, or SUBROUTINE, got %s",
                          token_type_name(p.cur.type));
        }
        skip_newlines(&p);
    }

    ParseResult result;
    result.units = (ASTNode**)units.items;
    result.nunits = units.count;
    if (p.has_error) {
        result.error = my_strdup(p.error_msg);
        result.error_line = p.error_line;
    } else {
        result.error = NULL;
        result.error_line = 0;
    }

    token_free(&p.cur);
    token_free(&p.prev);

    return result;
}

void parse_result_free(ParseResult *result) {
    for (int i = 0; i < result->nunits; i++) {
        ast_free(result->units[i]);
    }
    free(result->units);
    free(result->error);
}
