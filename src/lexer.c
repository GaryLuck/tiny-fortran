#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdio.h>
#include "lexer.h"

void lexer_init(Lexer *lex, const char *source) {
    lex->source = source;
    lex->pos = 0;
    lex->length = (int)strlen(source);
    lex->line = 1;
}

static char peek(Lexer *lex) {
    if (lex->pos >= lex->length) return '\0';
    return lex->source[lex->pos];
}

static char advance(Lexer *lex) {
    char c = lex->source[lex->pos++];
    if (c == '\n') lex->line++;
    return c;
}

static char *str_dup(const char *s) {
    size_t len = strlen(s);
    char *d = malloc(len + 1);
    if (d) memcpy(d, s, len + 1);
    return d;
}

static char *str_ndup(const char *s, size_t n) {
    char *d = malloc(n + 1);
    if (d) { memcpy(d, s, n); d[n] = '\0'; }
    return d;
}

static void str_upper(char *s) {
    for (; *s; s++) *s = (char)toupper((unsigned char)*s);
}

static Token make_token(TokenType type, const char *text, int line) {
    Token t;
    t.type = type;
    t.text = str_dup(text);
    t.int_value = 0;
    t.line = line;
    return t;
}

static Token make_int_token(int value, const char *text, int line) {
    Token t;
    t.type = TOK_INT_LIT;
    t.text = str_dup(text);
    t.int_value = value;
    t.line = line;
    return t;
}

static Token make_error(const char *msg, int line) {
    Token t;
    t.type = TOK_ERROR;
    t.text = str_dup(msg);
    t.int_value = 0;
    t.line = line;
    return t;
}

typedef struct {
    const char *word;
    TokenType type;
} Keyword;

static const Keyword keywords[] = {
    {"PROGRAM",    TOK_PROGRAM},
    {"END",        TOK_END},
    {"FUNCTION",   TOK_FUNCTION},
    {"SUBROUTINE", TOK_SUBROUTINE},
    {"INTEGER",    TOK_INTEGER},
    {"INTENT",     TOK_INTENT},
    {"IN",         TOK_IN},
    {"INOUT",      TOK_INOUT},
    {"IF",         TOK_IF},
    {"THEN",       TOK_THEN},
    {"ELSE",       TOK_ELSE},
    {"ELSEIF",     TOK_ELSEIF},
    {"DO",         TOK_DO},
    {"WHILE",      TOK_WHILE},
    {"PRINT",      TOK_PRINT},
    {"READ",       TOK_READ},
    {"CALL",       TOK_CALL},
    {"RETURN",     TOK_RETURN},
    {"STOP",       TOK_STOP},
    {"EXIT",       TOK_EXIT},
    {"CYCLE",      TOK_CYCLE},
    {NULL,         TOK_EOF}
};

static TokenType lookup_keyword(const char *upper) {
    for (int i = 0; keywords[i].word != NULL; i++) {
        if (strcmp(upper, keywords[i].word) == 0) {
            return keywords[i].type;
        }
    }
    return TOK_IDENT;
}

Token lexer_next(Lexer *lex) {
    /* Skip spaces and tabs (not newlines) */
    while (lex->pos < lex->length) {
        char c = peek(lex);
        if (c == ' ' || c == '\t' || c == '\r') {
            advance(lex);
        } else {
            break;
        }
    }

    if (lex->pos >= lex->length) {
        return make_token(TOK_EOF, "", lex->line);
    }

    int line = lex->line;
    char c = peek(lex);

    /* Newline */
    if (c == '\n') {
        advance(lex);
        return make_token(TOK_NEWLINE, "\\n", line);
    }

    /* Comment: skip to end of line */
    if (c == '!') {
        while (lex->pos < lex->length && peek(lex) != '\n') {
            advance(lex);
        }
        /* Return next token (the newline or EOF) */
        return lexer_next(lex);
    }

    /* String literal */
    if (c == '"' || c == '\'') {
        char quote = c;
        advance(lex);
        int start = lex->pos;
        while (lex->pos < lex->length && peek(lex) != quote && peek(lex) != '\n') {
            advance(lex);
        }
        int len = lex->pos - start;
        char *text = str_ndup(lex->source + start, (size_t)len);
        if (lex->pos < lex->length && peek(lex) == quote) {
            advance(lex); /* consume closing quote */
        }
        Token t;
        t.type = TOK_STR_LIT;
        t.text = text;
        t.int_value = 0;
        t.line = line;
        return t;
    }

    /* Integer literal */
    if (isdigit((unsigned char)c)) {
        int start = lex->pos;
        while (lex->pos < lex->length && isdigit((unsigned char)peek(lex))) {
            advance(lex);
        }
        int len = lex->pos - start;
        char *text = str_ndup(lex->source + start, (size_t)len);
        int val = atoi(text);
        Token t = make_int_token(val, text, line);
        free(text);
        t.text = str_ndup(lex->source + start, (size_t)len);
        return t;
    }

    /* Identifier or keyword */
    if (isalpha((unsigned char)c) || c == '_') {
        int start = lex->pos;
        while (lex->pos < lex->length &&
               (isalnum((unsigned char)peek(lex)) || peek(lex) == '_')) {
            advance(lex);
        }
        int len = lex->pos - start;
        char *text = str_ndup(lex->source + start, (size_t)len);
        char *upper = str_dup(text);
        str_upper(upper);
        TokenType type = lookup_keyword(upper);
        /* For ELSEIF, also handle "ELSE IF" — but the lexer sees them as two tokens.
           We handle "ELSE IF" in the parser instead. So ELSEIF keyword only matches
           the single-word form. */
        Token t = make_token(type, upper, line);
        free(text);
        free(upper);
        return t;
    }

    /* Dot-operators: .AND. .OR. .NOT. */
    if (c == '.') {
        int start = lex->pos;
        advance(lex); /* consume '.' */
        if (lex->pos < lex->length && isalpha((unsigned char)peek(lex))) {
            int wstart = lex->pos;
            while (lex->pos < lex->length && isalpha((unsigned char)peek(lex))) {
                advance(lex);
            }
            if (lex->pos < lex->length && peek(lex) == '.') {
                advance(lex); /* consume trailing '.' */
                int wlen = lex->pos - wstart - 1;
                char *word = str_ndup(lex->source + wstart, (size_t)wlen);
                str_upper(word);
                TokenType type = TOK_ERROR;
                if (strcmp(word, "AND") == 0) type = TOK_AND;
                else if (strcmp(word, "OR") == 0) type = TOK_OR;
                else if (strcmp(word, "NOT") == 0) type = TOK_NOT;
                else if (strcmp(word, "TRUE") == 0) {
                    free(word);
                    return make_int_token(1, ".TRUE.", line);
                }
                else if (strcmp(word, "FALSE") == 0) {
                    free(word);
                    return make_int_token(0, ".FALSE.", line);
                }
                free(word);
                if (type != TOK_ERROR) {
                    int len = lex->pos - start;
                    char *txt = str_ndup(lex->source + start, (size_t)len);
                    str_upper(txt);
                    Token t = make_token(type, txt, line);
                    free(txt);
                    return t;
                }
            }
        }
        /* Not a valid dot-operator, rewind */
        lex->pos = start + 1;
        return make_error("Unexpected '.'", line);
    }

    /* Two-character operators */
    advance(lex);
    if (lex->pos < lex->length) {
        char c2 = peek(lex);

        if (c == ':' && c2 == ':') {
            advance(lex);
            return make_token(TOK_DCOLON, "::", line);
        }
        if (c == '=' && c2 == '=') {
            advance(lex);
            return make_token(TOK_EQ, "==", line);
        }
        if (c == '/' && c2 == '=') {
            advance(lex);
            return make_token(TOK_NE, "/=", line);
        }
        if (c == '<' && c2 == '=') {
            advance(lex);
            return make_token(TOK_LE, "<=", line);
        }
        if (c == '>' && c2 == '=') {
            advance(lex);
            return make_token(TOK_GE, ">=", line);
        }
    }

    /* Single-character operators */
    switch (c) {
        case '+': return make_token(TOK_PLUS, "+", line);
        case '-': return make_token(TOK_MINUS, "-", line);
        case '*': return make_token(TOK_STAR, "*", line);
        case '/': return make_token(TOK_SLASH, "/", line);
        case '<': return make_token(TOK_LT, "<", line);
        case '>': return make_token(TOK_GT, ">", line);
        case '=': return make_token(TOK_ASSIGN, "=", line);
        case '(': return make_token(TOK_LPAREN, "(", line);
        case ')': return make_token(TOK_RPAREN, ")", line);
        case ',': return make_token(TOK_COMMA, ",", line);
    }

    /* Semicolons act as statement separators like newlines */
    if (c == ';') {
        return make_token(TOK_NEWLINE, ";", line);
    }

    char msg[64];
    snprintf(msg, sizeof(msg), "Unexpected character '%c'", c);
    return make_error(msg, line);
}

void token_free(Token *tok) {
    free(tok->text);
    tok->text = NULL;
}

const char *token_type_name(TokenType type) {
    switch (type) {
        case TOK_PROGRAM:    return "PROGRAM";
        case TOK_END:        return "END";
        case TOK_FUNCTION:   return "FUNCTION";
        case TOK_SUBROUTINE: return "SUBROUTINE";
        case TOK_INTEGER:    return "INTEGER";
        case TOK_INTENT:     return "INTENT";
        case TOK_IN:         return "IN";
        case TOK_INOUT:      return "INOUT";
        case TOK_IF:         return "IF";
        case TOK_THEN:       return "THEN";
        case TOK_ELSE:       return "ELSE";
        case TOK_ELSEIF:     return "ELSEIF";
        case TOK_DO:         return "DO";
        case TOK_WHILE:      return "WHILE";
        case TOK_PRINT:      return "PRINT";
        case TOK_READ:       return "READ";
        case TOK_CALL:       return "CALL";
        case TOK_RETURN:     return "RETURN";
        case TOK_STOP:       return "STOP";
        case TOK_EXIT:       return "EXIT";
        case TOK_CYCLE:      return "CYCLE";
        case TOK_INT_LIT:    return "INT_LIT";
        case TOK_STR_LIT:    return "STR_LIT";
        case TOK_IDENT:      return "IDENT";
        case TOK_PLUS:       return "PLUS";
        case TOK_MINUS:      return "MINUS";
        case TOK_STAR:       return "STAR";
        case TOK_SLASH:      return "SLASH";
        case TOK_EQ:         return "EQ";
        case TOK_NE:         return "NE";
        case TOK_LT:         return "LT";
        case TOK_GT:         return "GT";
        case TOK_LE:         return "LE";
        case TOK_GE:         return "GE";
        case TOK_AND:        return "AND";
        case TOK_OR:         return "OR";
        case TOK_NOT:        return "NOT";
        case TOK_ASSIGN:     return "ASSIGN";
        case TOK_LPAREN:     return "LPAREN";
        case TOK_RPAREN:     return "RPAREN";
        case TOK_COMMA:      return "COMMA";
        case TOK_DCOLON:     return "DCOLON";
        case TOK_NEWLINE:    return "NEWLINE";
        case TOK_EOF:        return "EOF";
        case TOK_ERROR:      return "ERROR";
    }
    return "UNKNOWN";
}
