#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

__attribute__((noreturn))
__attribute__((format(printf, 1, 2)))
void throw(const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    vfprintf(stderr, fmt, ap);
    fprintf(stderr, "\n");
    exit(2);
}

typedef struct Value {
    union {
        struct Value *ptr;
        uint64_t ival;
    };
} Value;

#define VAL_IS_INT(v) (v.ival & 1U)
#define VAL_IS_PTR(v) (!VAL_IS_INT(v.ival))

#define VAL_INT(v) (v.ival >> 1U)
#define VAL_PTR(v) (v.ptr)

typedef struct {
    Value car, cdr;
} Cell;

typedef struct {
    uint64_t capacity, length;
    Cell chunk[];
} CellChunk;

enum {
    CELL_INIT = 1,
};

static CellChunk *cells;

static void cell_init(void)
{
    cells = malloc(sizeof(CellChunk) + sizeof(Value) * CELL_INIT);
    if (cells == NULL)
        throw("chunk malloc failed");
    cells->capacity = CELL_INIT;
    cells->length = 0;
}

static Cell *cell_alloc(void)
{
    if (cells->capacity == cells->length) {
        cells->capacity *= 2;
        CellChunk *c = realloc(cells, cells->capacity);
        if (c == NULL)
            throw("chunk realloc failed");
        cells = c;
    }
    cells->length++;
    return &cells->chunk[cells->length-1];
}

typedef enum {
    TOK_OPAREN,
    TOK_CPAREN,
    TOK_INT,
    TOK_SYMBOL,
    TOK_INVALID
} Token;

static Token next_token(const char *buf)
{
    switch (buf[0]) {
    case '(':
        return TOK_OPAREN;
    case ')':
        return TOK_CPAREN;
    }
    return TOK_INVALID;
}

static Cell *parse_oparen(const char *buf)
{
    return (void *)(uintptr_t)(next_token(buf) == TOK_OPAREN);
}

static Cell *parse_cparen(const char *buf)
{
    return (void *)(uintptr_t)(next_token(buf) == TOK_CPAREN);
}

static Cell *parse_list(const char *buf)
{
    volatile const void *p = buf;
    return NULL;
}

static Cell *parse_expr(const char *buf)
{
    if (!parse_oparen(buf))
        throw("missing '('");
    Cell *l = parse_list(buf);
    if (parse_cparen(buf))
        throw("missing ')'");
    return l;
}

static Cell *parse(FILE *in)
{
    static char buf[1024*1024]; // aho ;)
    char *ret = fgets(buf, sizeof(buf), in);
    if (ret == NULL)
        throw("source too large");
    cell_init();
    return parse_expr(buf);
}

int main(int argc, char **argv)
{
    FILE *in = stdin;
    if (argc > 1) {
        in = fopen(argv[1], "r");
        if (in == NULL)
            throw("file %s not found", argv[1]);
    }
    void *p = parse(in);
    return !p;
}
