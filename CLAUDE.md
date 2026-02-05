# Tiny Fortran Interpreter

A tree-walking interpreter in C for a minimal but Turing-complete subset of Fortran.

## Build

Requires GCC and Make. Build via WSL2 on Windows:

```bash
wsl bash -c "cd '/mnt/c/Users/garyl/onedrive/documents/github/tiny-fortran' && make"
```

Or on Linux/macOS:

```bash
make
```

Produces `tinyfortran` (or `tinyfortran.exe` on Windows).

- `make clean` removes build artifacts
- Compiler flags: `-Wall -Wextra -std=c99 -pedantic`

## Usage

```bash
./tinyfortran <source.f90>
```

## Architecture

```
Source (.f90) → Lexer → Tokens → Parser → AST → Interpreter → Output
```

- **src/lexer.h/c** — Case-insensitive tokenizer, line-oriented, handles `!` comments
- **src/ast.h/c** — Tagged-union AST nodes with constructors and `ast_free()`
- **src/parser.h/c** — Recursive descent parser with precedence climbing for expressions
- **src/interpreter.h/c** — Tree-walking evaluator with scoped environments and call stack
- **src/main.c** — Entry point, file reading

## Supported Language

### Program structure
- `PROGRAM name ... END PROGRAM`
- `INTEGER FUNCTION name(args) ... END FUNCTION`
- `SUBROUTINE name(args) ... END SUBROUTINE`

### Declarations
- `INTEGER :: var1, var2`
- `INTEGER, INTENT(IN) :: arg`
- `INTEGER, INTENT(INOUT) :: arg`

### Statements
- Assignment, IF/ELSE IF/ELSE/END IF, DO loops, DO WHILE
- PRINT *, READ *, CALL, RETURN, STOP, EXIT, CYCLE

### Expressions
- Integer literals, variables, arithmetic (`+ - * /`), comparisons (`== /= < > <= >=`)
- Logical operators (`.AND.` `.OR.` `.NOT.`), parentheses, function calls
- Intrinsics: `MOD(a, b)`, `ABS(a)`

## Examples

```bash
./tinyfortran examples/hello.f90       # Hello world
./tinyfortran examples/factorial.f90   # Recursive factorial
./tinyfortran examples/fibonacci.f90   # Fibonacci sequence
./tinyfortran examples/fizzbuzz.f90    # FizzBuzz
./tinyfortran examples/gcd.f90         # GCD algorithm
```
