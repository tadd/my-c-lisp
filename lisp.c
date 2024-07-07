#include <ctype.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#include "utils.h"

#define throw(fmt, ...) \
    throw("%s:%d of %s: " fmt, __FILE__, __LINE__, __func__ __VA_OPT__(,) __VA_ARGS__)

typedef struct Pair Pair;

typedef union {
    Pair *pair;
    uintptr_t raw;
} Value;

// singleton
static const Value VALUE_NIL = (Value){ .pair = NULL };

struct Pair {
    Value car, cdr;
};

static inline bool value_is_int(Value v)
{
    return (v.raw & 1U) != 0;
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
    return (int64_t)(v.raw >> 1U);
}

static inline uintptr_t int_to_value_raw(int64_t i)
{
    return (((uintptr_t) i) << 1U) | 1U;
}

typedef struct {
    uintptr_t capacity, length;
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
    TTYPE_DOT,
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
    TOK_DOT = { .type = TTYPE_DOT },
    TOK_EOF = { .type = TTYPE_EOF };
#define TOK_INT(i) ((Token){ .type = TTYPE_INT,  .value = { .raw = int_to_value_raw(i) } })

typedef struct {
    char buf[1024*1024]; // aho ;)
    const char *s;
    Token prev_token;
} Parser;

static Token get_token_int(Parser *p)
{
    char *endp;
    int64_t i = strtoll(p->s, &endp, 10);
    if (p->s == endp)
        error("expected integer but got nothing in '%s'", p->s);
    p->s = endp;
    return TOK_INT(i);
}

static Token get_token(Parser *p)
{
    if (p->prev_token.type != TTYPE_EOF)  {
        Token t = p->prev_token;
        p->prev_token = TOK_EOF;
        return t;
    }
    while (isspace(*p->s))
        p->s++;

    switch (*p->s) {
    case '(':
        p->s++;
        return TOK_LPAREN;
    case ')':
        p->s++;
        return TOK_RPAREN;
    case '.':
        p->s++;
        return TOK_DOT;
    case '\0':
        return TOK_EOF;
    default:
        return get_token_int(p);
    }
}

static void unget_token(Parser *p, Token t)
{
    p->prev_token = t;
}

static inline bool got_eof(Parser *p)
{
    return p->s[0] == '\0';
}

static Value cons(Value car, Value cdr)
{
    Pair *c = pair_alloc();
    c->car = car;
    c->cdr = cdr;
    return (Value) { .pair = c };
}

static Value parse_expr(Parser *p);

static const char *token_stringify(Token t)
{
    switch (t.type) {
    case TTYPE_LPAREN:
        return "(";
    case TTYPE_RPAREN:
        return ")";
    case TTYPE_DOT:
        return ".";
    case TTYPE_INT:
        return "integer";
    case TTYPE_EOF:
        break;
    }
    return "EOF";
}

static Value parse_list(Parser *p)
{
    Token t = get_token(p);
    if (t.type == TTYPE_RPAREN)
        return VALUE_NIL;
    unget_token(p, t);
    Value car = parse_expr(p), cdr;
    t = get_token(p);
    if (t.type == TTYPE_DOT) {
        cdr = parse_expr(p);
        t = get_token(p);
        if (t.type != TTYPE_RPAREN)
            error("expected ')' but got '%s'", token_stringify(t));
    } else {
        unget_token(p, t);
        cdr = parse_list(p);
    }
    return cons(car, cdr);
}

static Value parse_expr(Parser *p)
{
    Token t = get_token(p);
    switch (t.type) {
    case TTYPE_LPAREN:
        return parse_list(p); // parse til ')'
    case TTYPE_RPAREN:
        error("expected expression but got ')'");
    case TTYPE_DOT:
        error("expected expression but got '.'");
    case TTYPE_INT:
        return t.value;
    case TTYPE_EOF:
        break;
    }
    return VALUE_NIL; // dummy
}

static Parser *parser_new(void)
{
    Parser *p = xmalloc(sizeof(Parser));
    p->s = p->buf;
    p->prev_token = TOK_EOF; // we use this since we never postpone EOF things
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
    for (;;) {
        Pair *p = v.pair;
        print(p->car);
        v = p->cdr;
        if (value_is_nil(v))
            break;
        printf(" ");
        if (value_is_atom(v)) {
            printf(". ");
            print_atom(v);
            break;
        }
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
        error("source invalid or too large");
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
            error("file %s not found", argv[1]);
    }
    Value v = parse(in);
    eval(v);
    return 0;
}
