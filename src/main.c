#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "lexer.h"
#include "parser.h"
#include "interpreter.h"

static char *read_file(const char *path) {
    FILE *f = fopen(path, "rb");
    if (!f) {
        fprintf(stderr, "Error: Cannot open file '%s'\n", path);
        return NULL;
    }

    fseek(f, 0, SEEK_END);
    long size = ftell(f);
    fseek(f, 0, SEEK_SET);

    char *buf = malloc((size_t)size + 1);
    if (!buf) {
        fprintf(stderr, "Error: Out of memory\n");
        fclose(f);
        return NULL;
    }

    size_t nread = fread(buf, 1, (size_t)size, f);
    buf[nread] = '\0';
    fclose(f);
    return buf;
}

static void print_usage(const char *prog) {
    fprintf(stderr, "Tiny Fortran Interpreter\n");
    fprintf(stderr, "Usage: %s <source.f90>\n", prog);
}

int main(int argc, char *argv[]) {
    if (argc < 2) {
        print_usage(argv[0]);
        return 1;
    }

    char *source = read_file(argv[1]);
    if (!source) return 1;

    /* Parse */
    ParseResult pr = parse(source);
    if (pr.error) {
        fprintf(stderr, "Parse error at line %d: %s\n", pr.error_line, pr.error);
        parse_result_free(&pr);
        free(source);
        return 1;
    }

    if (pr.nunits == 0) {
        fprintf(stderr, "Error: No program units found\n");
        parse_result_free(&pr);
        free(source);
        return 1;
    }

    /* Interpret */
    Interpreter *interp = interp_new();
    interp_register(interp, pr.units, pr.nunits);
    int result = interp_run(interp);

    interp_free(interp);
    parse_result_free(&pr);
    free(source);

    return result;
}
