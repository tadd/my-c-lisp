#include <ctype.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "lisp.h"
#include "utils.h"

#define error(fmt, ...) \
    error("%s:%d of %s: " fmt, __FILE__, __LINE__, __func__ __VA_OPT__(,) __VA_ARGS__)

typedef enum {
    TAG_PAIR,
    TAG_STR,
} ValueTag;

struct Pair {
    ValueTag tag; // common
    Value car, cdr;
};

typedef struct {
    ValueTag tag;
    const char *body;
} String;

#define VALUE_TAG(v) (*(ValueTag*)(v))
#define PAIR(v) ((Pair *) v)
#define STRING(v) ((String *) v)

// singletons
static const Pair PAIR_NIL = { .tag = TAG_PAIR, .car = 0, .cdr = 0 };
const Value Qnil = (uintptr_t) &PAIR_NIL;

inline bool value_is_int(Value v)
{
    return (v & 1U) != 0;
}

inline bool value_is_string(Value v)
{
    return !value_is_int(v) && VALUE_TAG(v) == TAG_STR;
}

inline bool value_is_symbol(Value v ATTR_UNUSED)
{
    return false;
}

inline bool value_is_atom(Value v)
{
    return value_is_int(v) || VALUE_TAG(v) != TAG_PAIR;
}

inline bool value_is_pair(Value v)
{
    return !value_is_int(v) && VALUE_TAG(v) == TAG_PAIR;
}

inline bool value_is_nil(Value v)
{
    return v == Qnil;
}

inline int64_t value_to_int(Value v)
{
    return (int64_t)(v >> 1U);
}

inline Value value_of_int(int64_t i)
{
    return (((uintptr_t) i) << 1U) | 1U;
}

inline Value value_of_string(const char *s)
{
    String *str = xmalloc(sizeof(String));
    str->tag = TAG_STR;
    str->body = xstrdup(s);
    return (Value) str;
}

inline const char *value_to_string(Value v)
{
    return STRING(v)->body;
}

typedef enum {
    TTYPE_LPAREN,
    TTYPE_RPAREN,
    TTYPE_INT,
    TTYPE_DOT,
    TTYPE_STR,
    TTYPE_EOF
} TokenType;

typedef struct {
    TokenType type;
    Value value;
} Token;


#define TOKEN(t) { .type = TTYPE_ ## t }
// singletons
static const Token
    TOK_LPAREN = TOKEN(LPAREN),
    TOK_RPAREN = TOKEN(RPAREN),
    TOK_DOT = TOKEN(DOT),
    TOK_EOF = TOKEN(EOF);
// and ctor
#define TOK_INT(i) ((Token){ .type = TTYPE_INT, .value = value_of_int(i) })
#define TOK_STR(s) ((Token){ .type = TTYPE_STR, .value = value_of_string(s) })

typedef struct {
    FILE *in;
    Token prev_token;
} Parser;

static Token get_token_int(Parser *p)
{
    int64_t i;
    int n = fscanf(p->in, "%ld", &i);
    if (n != 1)
        error("expected integer but got nothing");
    return TOK_INT(i);
}

static Token get_token_string(Parser *p)
{
    char buf[BUFSIZ], *pbuf = buf, *end = pbuf + sizeof(buf) - 2;
    for (;;) {
        int c = fgetc(p->in);
        if (c == '"')
            break;
        if (c == '\\') {
            c = fgetc(p->in);
            if (c != '\\' && c != '"')
                error("expected '\\' or '\"' in string literal but got '%c'", c);
        }
        if (pbuf == end)
            error("expected string literal but too long: \"%s...\"", pbuf);
        *pbuf++ = c;
    }
    *pbuf = '\0';
    return TOK_STR(buf);
}

static Token get_token(Parser *p)
{
    if (p->prev_token.type != TTYPE_EOF)  {
        Token t = p->prev_token;
        p->prev_token = TOK_EOF;
        return t;
    }

    int c;
    do {
        c = fgetc(p->in);
    } while (isspace(c));

    switch (c) {
    case '(':
        return TOK_LPAREN;
    case ')':
        return TOK_RPAREN;
    case '.':
        return TOK_DOT;
    case '"':
        return get_token_string(p);
    case EOF:
        return TOK_EOF;
    default:
        break;
    }
    if (isdigit(c)) {
        c = ungetc(c, p->in);
        if (c == EOF)
            error("ungetc");
        return get_token_int(p);
    }
    error("got unexpected char '%c'", c);
}

static void unget_token(Parser *p, Token t)
{
    p->prev_token = t;
}

static inline bool got_eof(Parser *p)
{
    return feof(p->in);
}

Value cons(Value car, Value cdr)
{
    Pair *c = xmalloc(sizeof(Pair));
    c->tag = TAG_PAIR;
    c->car = car;
    c->cdr = cdr;
    return (Value) c;
}

Value car(Value v)
{
    return PAIR(v)->car;
}

Value cdr(Value v)
{
    return PAIR(v)->cdr;
}

static Value parse_expr(Parser *p);

static const char *token_stringify(Token t)
{
    static char buf[BUFSIZ];

    switch (t.type) {
    case TTYPE_LPAREN:
        return "(";
    case TTYPE_RPAREN:
        return ")";
    case TTYPE_DOT:
        return ".";
    case TTYPE_INT:
        snprintf(buf, sizeof(buf), "%ld", value_to_int(t.value));
        return buf;
    case TTYPE_STR:
    case TTYPE_EOF:
        break;
    }
    return "EOF";
}

static Value parse_dotted_pair(Parser *p)
{
    Value e = parse_expr(p);
    Token t = get_token(p);
    if (t.type != TTYPE_RPAREN)
        error("expected ')' but got '%s'", token_stringify(t));
    return e;
}

static Value parse_list(Parser *p)
{
    Token t = get_token(p);
    if (t.type == TTYPE_RPAREN)
        return Qnil;
    unget_token(p, t);
    Value car = parse_expr(p), cdr;
    t = get_token(p);
    if (t.type == TTYPE_EOF)
        error("expected ')' but got '%s'", token_stringify(t));
    if (t.type == TTYPE_DOT) {
        cdr = parse_dotted_pair(p);
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
    case TTYPE_STR:
    case TTYPE_INT:
        return t.value;
    case TTYPE_EOF:
        break;
    }
    return Qnil; // dummy
}

static Parser *parser_new(FILE *in)
{
    Parser *p = xmalloc(sizeof(Parser));
    p->in = in;
    p->prev_token = TOK_EOF; // we use this since we never postpone EOF things
    return p;
}

Value eval(Value v)
{
    return v; // dummy
}

static void print_atom(FILE *f, Value v)
{
    if (value_is_int(v))
        fprintf(f, "%ld", value_to_int(v));
    else if (value_is_string(v))
        fprintf(f, "\"%s\"", value_to_string(v));
}

static void fprint(FILE* f, Value v);

static void print_list(FILE *f, Value v)
{
    for (;;) {
        Pair *p = PAIR(v);
        fprint(f, p->car);
        v = p->cdr;
        if (value_is_nil(v))
            break;
        fprintf(f, " ");
        if (value_is_atom(v)) {
            fprintf(f, ". ");
            print_atom(f, v);
            break;
        }
    }
}

static void print_pair(FILE *f, Value v)
{
    fprintf(f, "(");
    if (!value_is_nil(v))
        print_list(f, v);
    fprintf(f, ")");
}

static void fprint(FILE* f, Value v)
{
    if (value_is_atom(v))
        print_atom(f, v);
    else
        print_pair(f, v);
}

void print(Value v)
{
    fprint(stdout, v);
}

char *stringify(Value v)
{
    char *s;
    size_t size;
    FILE *stream = open_memstream(&s, &size);
    if (stream == NULL)
        return NULL;
    fprint(stream, v);
    fclose(stream);
    return s;
}

static Value reverse(Value v)
{
    if (value_is_nil(v))
        return v;
    Value next = PAIR(v)->cdr;
    if (value_is_nil(next))
        return v;

    Value prev = Qnil;
    for (;;) {
        next = PAIR(v)->cdr;
        PAIR(v)->cdr = prev;
        if (value_is_nil(next))
            break;
        prev = v;
        v = next;
    }
    return v;
}

Value parse(FILE *in)
{
    Parser *p = parser_new(in);
    Value v = Qnil;
    for (;;) {
        Value expr = parse_expr(p);
        if (value_is_nil(expr) && got_eof(p))
            break;
        v = cons(expr, v);
    }
    free(p);
    return reverse(v);
}

Value parse_expr_from_string(const char *in)
{
    FILE *f = fmemopen((char *)in, strlen(in), "r");
    Parser *p = parser_new(f);
    Value v = parse_expr(p);
    free(p);
    fclose(f);
    return v;
}
