CC = gcc
CFLAGS = -Wall -Wextra -std=c99 -pedantic
SRCDIR = src
BUILDDIR = build

SOURCES = $(SRCDIR)/main.c $(SRCDIR)/lexer.c $(SRCDIR)/ast.c $(SRCDIR)/parser.c $(SRCDIR)/interpreter.c
OBJECTS = $(BUILDDIR)/main.o $(BUILDDIR)/lexer.o $(BUILDDIR)/ast.o $(BUILDDIR)/parser.o $(BUILDDIR)/interpreter.o

ifeq ($(OS),Windows_NT)
    TARGET = tinyfortran.exe
    RM = del /Q
    RMDIR = rmdir /S /Q
    MKDIR = if not exist $(BUILDDIR) mkdir $(BUILDDIR)
else
    TARGET = tinyfortran
    RM = rm -f
    RMDIR = rm -rf
    MKDIR = mkdir -p $(BUILDDIR)
endif

all: $(TARGET)

$(TARGET): $(OBJECTS)
	$(CC) $(CFLAGS) -o $@ $^

$(BUILDDIR)/%.o: $(SRCDIR)/%.c | $(BUILDDIR)
	$(CC) $(CFLAGS) -c -o $@ $<

$(BUILDDIR):
	$(MKDIR)

clean:
ifeq ($(OS),Windows_NT)
	-$(RMDIR) $(BUILDDIR)
	-$(RM) $(TARGET)
else
	$(RMDIR) $(BUILDDIR)
	$(RM) $(TARGET)
endif

.PHONY: all clean
