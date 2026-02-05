#ifndef LEXER_H
#define LEXER_H

typedef enum {
    /* Keywords */
    TOK_PROGRAM, TOK_END, TOK_FUNCTION, TOK_SUBROUTINE,
    TOK_INTEGER, TOK_INTENT, TOK_IN, TOK_INOUT,
    TOK_IF, TOK_THEN, TOK_ELSE, TOK_ELSEIF,
    TOK_DO, TOK_WHILE,
    TOK_PRINT, TOK_READ,
    TOK_CALL, TOK_RETURN, TOK_STOP,
    TOK_EXIT, TOK_CYCLE,

    /* Literals and identifiers */
    TOK_INT_LIT,    /* integer literal */
    TOK_STR_LIT,    /* string literal */
    TOK_IDENT,      /* identifier */

    /* Operators */
    TOK_PLUS, TOK_MINUS, TOK_STAR, TOK_SLASH,
    TOK_EQ,         /* == */
    TOK_NE,         /* /= */
    TOK_LT,         /* < */
    TOK_GT,         /* > */
    TOK_LE,         /* <= */
    TOK_GE,         /* >= */
    TOK_AND,        /* .AND. */
    TOK_OR,         /* .OR. */
    TOK_NOT,        /* .NOT. */
    TOK_ASSIGN,     /* = */

    /* Punctuation */
    TOK_LPAREN, TOK_RPAREN,
    TOK_COMMA,
    TOK_DCOLON,     /* :: */

    /* Special */
    TOK_NEWLINE,
    TOK_EOF,
    TOK_ERROR
} TokenType;

typedef struct {
    TokenType type;
    char *text;       /* token text (allocated) */
    int int_value;    /* for TOK_INT_LIT */
    int line;         /* source line number */
} Token;

typedef struct {
    const char *source;   /* source text */
    int pos;              /* current position */
    int length;           /* source length */
    int line;             /* current line number */
} Lexer;

void lexer_init(Lexer *lex, const char *source);
Token lexer_next(Lexer *lex);
void token_free(Token *tok);
const char *token_type_name(TokenType type);

#endif
