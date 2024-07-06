#include <ctype.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#include "utils.h"

typedef struct Cell Cell;

typedef struct {
    union {
        Cell *cell;
        uint64_t ival;
    };
} Value;

// signleton
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

static inline bool value_is_nil(Value v)
{
    return value_is_cell(v) && v.cell == NULL;
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
    TTYPE_LPAREN,
    TTYPE_RPAREN,
    TTYPE_INT,
//  TTYPE_SYMBOL,
    TTYPE_EOF,
    TTYPE_INVALID
} TokenType;

typedef struct {
    TokenType type;
    Value value;
} Token;


// singletons
static const Token
    TOK_LPAREN = { .type = TTYPE_LPAREN },
    TOK_RPAREN = { .type = TTYPE_RPAREN },
    TOK_EOF = { .type = TTYPE_EOF },
    TOK_INVALID = { .type = TTYPE_INVALID };
#define TOK_INT(i) ((Token){ .type = TTYPE_INT,  .value = { .ival = int_to_value_ival(i) }})

typedef struct {
    char buf[1024*1024]; // aho ;)
    const char *p;
} Parser;

static Token get_token_int(Parser *p)
{
    const char *beg = p->p;
    while (isdigit(*p->p))
        p->p++;
    if (beg == p->p)
        return TOK_INVALID;
    char *endp;
    int64_t i = strtoll(beg, &endp, 10);
    p->p = endp;
    return TOK_INT(i);
}

static Token get_token(Parser *p)
{
    while (isspace(*p->p))
        p->p++;

    switch (*p->p) {
    case '(':
        p->p++;
        return TOK_LPAREN;
    case ')':
        p->p++;
        return TOK_RPAREN;
    case '\0':
        return TOK_EOF;
    default:
        return get_token_int(p);
    }
}

static Token peek_token_int(const char *peek)
{
    const char *beg = peek;
    while (isdigit(*peek))
        peek++;
    if (beg == peek)
        return TOK_INVALID;
    char *endp;
    int64_t i = strtoll(beg, &endp, 10);
    peek = endp;
    return TOK_INT(i);
}

ATTR_UNUSED
static Token peek_token(Parser *p)
{
    const char *peek = p->p;

    while (isspace(*peek))
        peek++;

    switch (*peek) {
    case '(':
        peek++;
        return TOK_LPAREN;
    case ')':
        peek++;
        return TOK_RPAREN;
    case '\0':
        return TOK_EOF;
    default:
        return peek_token_int(peek);
    }
}

static Value parse_list(Parser *p ATTR_UNUSED)
{
    return (Value){ .ival = 0 }; // dummy here
}

static Value parse_expr(Parser *p)
{
    Token t = get_token(p);
    switch (t.type) {
    case TTYPE_LPAREN:
        return parse_list(p);
    case TTYPE_RPAREN:
        throw("expected expression but got ')'");
    case TTYPE_INT:
        return t.value;
    case TTYPE_EOF:
        return VALUE_EOF;
    case TTYPE_INVALID:
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

static void print(Value v)
{
    if (value_is_int(v)) {
        printf("%ld", value_to_int(v));
        return;
    }
    printf("(");
    if (!value_is_nil(v)) {
        Cell *c = v.cell;
        print(c->car);
        if (!value_is_nil(c->cdr)) {
            printf(" . ");
            print(c->cdr);
        }
    }
    printf(")");
}

static Value parse(FILE *in)
{
    Parser *p = parser_new();
    char *ret = fgets(p->buf, sizeof(p->buf), in);
    if (ret == NULL)
        throw("source invalid or too large");
    cell_init();
    Value v;
    for (;;) {
        v = parse_expr(p);
        if (value_is_eof(v))
            break;
        print(v);
        printf("\n");
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
    Value v = parse(in);
    eval(v);
    return 0;
}
