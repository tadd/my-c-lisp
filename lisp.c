#include <ctype.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#include "utils.h"

#define throw(fmt, ...) throw("%s:%d of %s: " fmt, __FILE__, __LINE__, __func__ __VA_OPT__(,) __VA_ARGS__)

typedef struct Pair Pair;

typedef struct {
    union {
        Pair *pair;
        uint64_t ival;
    };
} Value;

// singletons
static const Value VALUE_EOF = (Value){ .ival = INT64_MIN };
static const Value VALUE_NIL = (Value){ .pair = NULL };

struct Pair {
    Value car, cdr;
};

static inline bool value_is_int(Value v)
{
    return (v.ival & 1U) != 0;
}

static inline bool value_is_symbol(Value v ATTR_UNUSED)
{
    return false;
}

static inline bool value_is_atom(Value v)
{
    return value_is_int(v) || value_is_symbol(v);
}

static inline bool value_is_pair(Value v)
{
    return !value_is_atom(v);
}

static inline bool value_is_nil(Value v)
{
    return value_is_pair(v) && v.pair == NULL;
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
    Pair chunk[];
} PairChunk;

enum {
    PAIR_INIT = 1,
};

static PairChunk *pairs;

static void pair_init(void)
{
    pairs = xmalloc(sizeof(PairChunk) + sizeof(Pair) * PAIR_INIT);
    pairs->capacity = PAIR_INIT;
    pairs->length = 0;
}

static Pair *pair_alloc(void)
{
    if (pairs->capacity == pairs->length) {
        pairs->capacity *= 2;
        pairs = xrealloc(pairs, sizeof(PairChunk) + sizeof(Pair) * pairs->capacity);
    }
    return &pairs->chunk[pairs->length++];
}

typedef enum {
    TTYPE_LPAREN,
    TTYPE_RPAREN,
    TTYPE_INT,
//  TTYPE_SYMBOL,
    TTYPE_EOF
} TokenType;

typedef struct {
    TokenType type;
    Value value;
} Token;


// singletons
static const Token
    TOK_LPAREN = { .type = TTYPE_LPAREN },
    TOK_RPAREN = { .type = TTYPE_RPAREN },
    TOK_EOF = { .type = TTYPE_EOF };
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
        throw("expected integer but got nothing in '%s'", beg);
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
        throw("expected integer but got nothing in '%s'", beg);
    char *endp;
    int64_t i = strtoll(beg, &endp, 10);
    peek = endp;
    return TOK_INT(i);
}

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

static inline bool got_eof(Parser *p)
{
    return p->p[0] == '\0';
}

static Value cons(Value car, Value cdr)
{
    Pair *c = pair_alloc();
    c->car = car;
    c->cdr = cdr;
    return (Value) { .pair = c };
}

static Value parse_expr(Parser *p);

static Value parse_list_inner(Parser *p)
{
    Token t = peek_token(p);
    switch (t.type) {
    case TTYPE_RPAREN:
        return VALUE_NIL;
    case TTYPE_LPAREN:
    case TTYPE_INT:
        Value car = parse_expr(p);
        Value cdr = parse_list_inner(p);
        return cons(car, cdr);
    case TTYPE_EOF:
        break;
    }
    throw("expected expression list but got EOF");

}

static const char *token_stringify(Token t)
{
    switch (t.type) {
    case TTYPE_LPAREN:
        return "(";
    case TTYPE_RPAREN:
        return ")";
    case TTYPE_INT:
        return "integer";
    case TTYPE_EOF:
        break;
    }
    return "EOF";
}

static Value parse_expr(Parser *p)
{
    Token t = get_token(p);
    switch (t.type) {
    case TTYPE_LPAREN:
        Value inner = parse_list_inner(p);
        t = get_token(p);
        if (t.type != TTYPE_RPAREN)
            throw("expected ')' but got '%s'", token_stringify(t));
        return inner;
    case TTYPE_RPAREN:
        throw("expected expression but got ')'");
    case TTYPE_INT:
        return t.value;
    case TTYPE_EOF:
        break;
    }
    return VALUE_EOF; // dummy
}

static Parser *parser_new(void)
{
    Parser *p = xmalloc(sizeof(Parser));
    p->p = p->buf;
    return p;
}

static Value eval(Value v)
{
    return v; // dummy
}

static void print_atom(Value v)
{
    printf("%ld", value_to_int(v));
}

static void print(Value v);

static void print_list(Value v)
{
    Pair *c = v.pair;
    print(c->car);
    if (value_is_atom(c->cdr))
        print_atom(c->cdr);
    else if (!value_is_nil(c->cdr)) {
        printf(" ");
        print_list(c->cdr);
    }
}

static void print_pair(Value v)
{
    printf("(");
    if (!value_is_nil(v))
        print_list(v);
    printf(")");
}

static void print(Value v)
{
    if (value_is_atom(v))
        print_atom(v);
    else
        print_pair(v);
}

static Value parse(FILE *in)
{
    Parser *p = parser_new();
    char *ret = fgets(p->buf, sizeof(p->buf), in);
    if (ret == NULL)
        throw("source invalid or too large");
    pair_init();
    Value v;
    for (;;) {
        v = parse_expr(p);
        if (got_eof(p))
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
