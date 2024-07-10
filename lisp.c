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
    return !!(v & 1U);
}

static inline bool is_immediate(Value v)
{
    return !!(v & 0b111U); // expects 0b...000 for pointers
}

inline bool value_is_string(Value v)
{
    return !is_immediate(v) && VALUE_TAG(v) == TAG_STR;
}

inline bool value_is_symbol(Value v)
{
    return (v & 0b11U) == 0b10U;
}

inline bool value_is_pair(Value v)
{
    return !is_immediate(v) && VALUE_TAG(v) == TAG_PAIR;
}

inline bool value_is_atom(Value v)
{
    return !value_is_pair(v);
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

inline Symbol value_to_symbol(Value v)
{
    return (Symbol) (v >> 2U);
}

static Symbol intern(const char *s);
static const char *unintern(Symbol sym);

inline const char *value_to_string(Value v)
{
    if (value_is_symbol(v))
        return unintern(value_to_symbol(v));
    return STRING(v)->body;
}

inline Value value_of_symbol(const char *s)
{
    Symbol sym = intern(s);
    return (Value) (sym << 2U | 0b10U);
}

typedef enum {
    TTYPE_LPAREN,
    TTYPE_RPAREN,
    TTYPE_INT,
    TTYPE_DOT,
    TTYPE_STR,
    TTYPE_IDENT,
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
#define TOK_V(t, v) ((Token) { .type = TTYPE_ ## t, .value = v })
#define TOK_INT(i) TOK_V(INT, value_of_int(i))
#define TOK_STR(s) TOK_V(STR, value_of_string(s))
#define TOK_IDENT(s) TOK_V(IDENT, value_of_symbol(s))

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

static Symbol intern(const char *s)
{
    return *s; // dummy
}

static const char *unintern(Symbol sym)
{
    static char buf[BUFSIZ];
    snprintf(buf, sizeof(buf), "%c", (int) sym);
    return buf;
}

static inline bool is_peculiar_single(int c)
{
    return c == '+' || c == '-';
}

static Token get_token_ident_dots(Parser *p)
{
    int c2 = fgetc(p->in);
    int c3 = fgetc(p->in);
    if (!(c2 == '.' && c3 == '.'))
        error("expected '...' but got '.%c%c'", c2, c3);
    return TOK_IDENT("...");
}

static inline bool is_special_initial(int c)
{
    switch (c) {
    case '!':
    case '$':
    case '%':
    case '&':
    case '*':
    case '/':
    case ':':
    case '<':
    case '=':
    case '|':
    case '>':
    case '?':
    case '^':
    case '_':
    case '~':
        return true;
    default:
        break;
    }
    return false;
}

static inline bool is_initial(int c)
{
    return isalpha(c) || is_special_initial(c);
}

static inline bool is_special_subsequent(int c)
{
    switch (c) {
    case '+':
    case '-':
    case '.':
    case '@':
        return true;
    default:
        break;
    }
    return false;
}

static inline bool is_subsequent(int c)
{
    return is_initial(c) || isdigit(c) || is_special_subsequent(c);
}

static Token get_token_ident(Parser *p)
{
    int c = fgetc(p->in);
    if (c == '.')
        return get_token_ident_dots(p);

    char buf[BUFSIZ], *s = buf, *end = s + sizeof(buf);
    if (!is_initial(c))
        error("expected identifier but got '%c' as initial", c);
    *s++ = c;
    for (;;) {
        c = fgetc(p->in);
        if (!is_subsequent(c))
            break;
        *s++ = c;
        if (s == end)
            error("expected identifier but was too long");
    }
    ungetc(c, p->in);
    *s = '\0';
    return TOK_IDENT(buf);
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
        ungetc(c, p->in);
        return get_token_int(p);
    }
    if (is_peculiar_single(c)) {
        char ident[] = { c, '\0' };
        return TOK_IDENT(ident);
    }
    if (isalpha(c) || is_special_initial(c) || c == '.') {
        ungetc(c, p->in);
        return get_token_ident(p);
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

#define DEF_CXXR(x, y) \
    Value c##x##y##r(Value v) { return c##x##r(c##y##r(v)); }
#define DEF_CXXXR(x, y, z) DEF_CXXR(x, y##z)
#define DEF_CXXXXR(x, y, z, w) DEF_CXXXR(x, y, z##w)

DEF_CXXR(a, a)
DEF_CXXR(a, d)
DEF_CXXR(d, a)
DEF_CXXR(d, d)
DEF_CXXXR(a, a, a)
DEF_CXXXR(a, a, d)
DEF_CXXXR(a, d, a)
DEF_CXXXR(a, d, d)
DEF_CXXXR(d, a, a)
DEF_CXXXR(d, a, d)
DEF_CXXXR(d, d, a)
DEF_CXXXR(d, d, d)
DEF_CXXXXR(a, a, a, a)
DEF_CXXXXR(a, a, a, d)
DEF_CXXXXR(a, a, d, a)
DEF_CXXXXR(a, a, d, d)
DEF_CXXXXR(a, d, a, a)
DEF_CXXXXR(a, d, a, d)
DEF_CXXXXR(a, d, d, a)
DEF_CXXXXR(a, d, d, d)
DEF_CXXXXR(d, a, a, a)
DEF_CXXXXR(d, a, a, d)
DEF_CXXXXR(d, a, d, a)
DEF_CXXXXR(d, a, d, d)
DEF_CXXXXR(d, d, a, a)
DEF_CXXXXR(d, d, a, d)
DEF_CXXXXR(d, d, d, a)
DEF_CXXXXR(d, d, d, d)

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
        break;
    case TTYPE_IDENT:
        return value_to_string(t.value);
    case TTYPE_STR:
        snprintf(buf, sizeof(buf), "\"%s\"", STRING(t.value)->body);
        break;
    case TTYPE_EOF:
        return "EOF";
    }
    return buf;
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
    case TTYPE_IDENT:
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
    else if (value_is_symbol(v))
        fprintf(f, "'%s", value_to_string(v));
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
