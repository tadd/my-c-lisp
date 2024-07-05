#include <ctype.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#define ATTR_UNUSED __attribute__((unused))

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

static const Value VALUE_EOF = (Value){ .ival = INT64_MIN };

struct Cell {
    Value car, cdr;
};

static inline bool value_is_int(Value v)
{
    return v.ival & 1U;
}

static inline bool value_is_cell(Value v)
{
    return !value_is_int(v);
}

static inline bool value_is_eof(Value v)
{
    return v.ival == VALUE_EOF.ival;
}

static inline int64_t value_to_int(Value v)
{
    return (int64_t)(v.ival >> 1U);
}

static inline uint64_t int_to_value_ival(int64_t i)
{
    return (((uint64_t) i) << 1U) | 1U;
}

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

ATTR_UNUSED
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

static Token get_token_int(Parser *p, Value *v)
{
    const char *beg = p->p;
    while (isdigit(*p->p))
        p->p++;
    if (beg == p->p)
        return TOK_INVALID;
    int64_t i = strtoll(beg, NULL, 10);
    v->ival = int_to_value_ival(i);
    return TOK_INT;
}

static Token get_token(Parser *p, Value *v)
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
        return get_token_int(p, v);
    }
}

static Value parse_list(Parser *p ATTR_UNUSED)
{
    return (Value){ .ival = 0 };
}

static Value parse_expr(Parser *p)
{
    Value v;
    Token t = get_token(p, &v);
    switch (t) {
    case TOK_LPAREN:
        return parse_list(p);
    case TOK_RPAREN:
        throw("expected expression but got ')'");
    case TOK_INT:
    case TOK_SYMBOL:
        return v;
    case TOK_EOF:
        return VALUE_EOF;
    case TOK_INVALID:
        break;
    }
    throw("expected expression but got invalid string before '%s'", &p->p[-1]);
}

static Parser *parser_new(void)
{
    Parser *p = xmalloc(sizeof(Parser));
    p->p = p->buf;
    return p;
}

static Value eval(Value v)
{
    return v;
}

static void print(Value v ATTR_UNUSED)
{
    // void here for now
}

static Value parse(FILE *in)
{
    Parser *p = parser_new();
    char *ret = fgets(p->buf, sizeof(p->buf), in);
    if (ret == NULL)
        throw("source too large");
    cell_init();
    Value v;
    for (;;) {
        v = parse_expr(p);
        if (value_is_eof(v))
            break;
    }
    free(p);
    return v;
}

int main(int argc, char **argv)
{
    FILE *in = stdin;
    if (argc > 1) {
        in = fopen(argv[1], "r");
        if (in == NULL)
            throw("file %s not found", argv[1]);
    }
    ATTR_UNUSED Value v = parse(in);
    print(eval(v));
    return 0;
}
