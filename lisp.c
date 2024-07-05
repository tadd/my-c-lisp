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

typedef struct Cell Cell;

typedef struct {
    union {
        Cell *cell;
        uint64_t ival;
    };
} Value;

struct Cell {
    Value car, cdr;
};

#define VAL_IS_INT(v) (v.ival & 1U)
#define VAL_IS_CELL(v) (!VAL_IS_INT(v.ival))

#define VAL2INT(v) ((int64_t)(v.ival >> 1U))
#define INT2VAL(i) (((uint64_t)i)<<1U|1U)

typedef struct {
    uint64_t capacity, length;
    Cell chunk[];
} CellChunk;

enum {
    CELL_INIT = 1,
};

static CellChunk *cells;

__attribute__((malloc(free)))
static void *xmalloc(size_t size)
{
    void *p = malloc(size);
    if (p == NULL)
        throw("malloc %zu bytes failed", size);
    return p;
}

__attribute__((malloc(free)))
static void *xrealloc(void *p, size_t size)
{
    p = realloc(p, size);
    if (p == NULL)
        throw("realloc to %zu bytes failed", size);
    return p;
}

static void cell_init(void)
{
    cells = xmalloc(sizeof(CellChunk) + sizeof(Value) * CELL_INIT);
    cells->capacity = CELL_INIT;
    cells->length = 0;
}

static Cell *cell_alloc(void)
{
    if (cells->capacity == cells->length) {
        cells->capacity *= 2;
        cells = xrealloc(cells, sizeof(CellChunk) + sizeof(Value) * cells->capacity);
    }
    return &cells->chunk[cells->length++];
}

typedef enum {
    TOK_LPAREN,
    TOK_RPAREN,
    TOK_INT,
    TOK_SYMBOL,
    TOK_EOF,
    TOK_INVALID
} Token;

typedef struct {
    char buf[1024*1024]; // aho ;)
    const char *p;
} Parser;

static Token get_token(Parser *p)
{
    switch (*p->p) {
    case '(':
        p->p++;
        return TOK_LPAREN;
        break;
    case ')':
        p->p++;
        return TOK_RPAREN;
    case '\0':
        return TOK_EOF;
    default:
        return TOK_INVALID;
    }
}

static const char *token_stringify(Token t)
{
    switch (t) {
    case TOK_LPAREN:
        return "(";
    case TOK_RPAREN:
        return ")";
    case TOK_INT:
        return "<int>";
    case TOK_SYMBOL:
        return "<sym>";
    case TOK_EOF:
        return "\\0";
    case TOK_INVALID:
        break;
    }
    return "invalid";
}

static Token get_token_of(Parser *p, Token expected)
{
    Token t = get_token(p);
    if (t != expected) {
        throw("expected %s but got %s",
              token_stringify(expected), token_stringify(t));
    }
    return t;
}

static Cell *parse_list(Parser *p)
{
    return NULL;
}

static Cell *parse_expr(Parser *p)
{
    get_token_of(p, TOK_LPAREN);
    Cell *l = parse_list(p);
    get_token_of(p, TOK_RPAREN);
    return l;
}

static Parser *parser_new(void)
{
    Parser *p = malloc(sizeof(Parser));
    p->p = p->buf;
    return p;
}

static Cell *parse(FILE *in)
{
    Parser *p = parser_new();
    char *ret = fgets(p->buf, sizeof(p->buf), in);
    if (ret == NULL)
        throw("source too large");
    cell_init();
    Cell *expr = parse_expr(p);
    free(p);
    return expr;
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
