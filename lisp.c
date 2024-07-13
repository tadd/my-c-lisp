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
#define unexpected(exp, act, ...) \
    error("expected %s but got " act, exp __VA_OPT__(,) __VA_ARGS__)

typedef enum {
    TAG_PAIR,
    TAG_STR,
    TAG_FUNC,
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
#define FUNCTION(v) ((Function *) v)

typedef enum {
// immediate
    TYPE_INT,
    TYPE_SYMBOL,
// boxed (tagged)
    TYPE_PAIR,
    TYPE_STR,
    TYPE_FUNC,
} Type;

static const char *TYPE_NAMES[] = {
    [TYPE_INT] = "integer",
    [TYPE_SYMBOL] = "symbol",
    [TYPE_PAIR] = "pair",
    [TYPE_STR] = "string",
    [TYPE_FUNC] = "function",
};

// singletons
static const Pair PAIR_NIL = { .tag = TAG_PAIR, .car = 0, .cdr = 0 };
const Value Qnil = (Value) &PAIR_NIL;

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
    return (int64_t) v >> 1U;
}

inline Value value_of_int(int64_t i)
{
    return (Value) i << 1U | 1U;
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

#define ANYARGS /*empty*/
typedef Value (*CFunc)(ANYARGS);
typedef struct {
    CFunc cfunc;
    long arity;
} Function;

static Value value_of_func(CFunc cfunc, long arity)
{
    Function *f = xmalloc(sizeof(Function));
    f->cfunc = cfunc;
    f->arity = arity;
    return (Value) f;
}

static inline Type value_typeof(Value v)
{
    if (is_immediate(v))
        return value_is_int(v) ? TYPE_INT : TYPE_SYMBOL;
    switch (VALUE_TAG(v)) {
    case TAG_STR:
        return TYPE_STR;
    case TAG_PAIR:
        return TYPE_PAIR;
    case TAG_FUNC:
        return TYPE_FUNC;
    default:
        UNREACHABLE();
    }
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

static Token get_token_int(Parser *p, int sign)
{
    int64_t i;
    int n = fscanf(p->in, "%ld", &i);
    if (n != 1)
        unexpected("integer", "invalid string");
    return TOK_INT(sign * i);
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
                unexpected("'\\' or '\"' in string literal", "'%c'", c);
        }
        if (pbuf == end)
            unexpected("string literal", "too long: \"%s...\"", pbuf);
        *pbuf++ = c;
    }
    *pbuf = '\0';
    return TOK_STR(buf);
}

static Value symbol_names = Qnil;

static Symbol intern(const char *name)
{
    long i;
    Value l = symbol_names;
    Value last = Qnil;
    // find
    for (i = 0; l != Qnil; l = cdr(l), i++) {
        Value v = car(l);
        if (strcmp(STRING(v)->body, name) == 0)
            return i;
        last = l;
    }
    // or put at `i`
    Value s = value_of_string(name);
    Value next = cons(s, Qnil);
    if (last == Qnil)
        symbol_names = next;
    else
        PAIR(last)->cdr = next;
    return i;
}

static const char *name_nth(Value list, long n)
{
    for (long i = 0; i < n; i++) {
        list = cdr(list);
        if (list == Qnil)
            return NULL;
    }
    Value name = car(list);
    return STRING(name)->body;
}

static const char *unintern(Symbol sym)
{
    const char *name = name_nth(symbol_names, (long) sym);
    if (name == NULL)
        error("symbol %lu not found", sym);
    return name;
}

static inline bool is_special_initial(int c)
{
    switch (c) {
    case '!': case '$': case '%': case '&': case '*':
    case '/': case ':': case '<': case '=': case '|':
    case '>': case '?': case '^': case '_': case '~':
        return true;
    default:
        return false;
    }
}

static inline bool is_initial(int c)
{
    return isalpha(c) || is_special_initial(c);
}

static inline bool is_special_subsequent(int c)
{
    switch (c) {
    case '+': case '-': case '.': case '@':
        return true;
    default:
        return false;
    }
}

static inline bool is_subsequent(int c)
{
    return is_initial(c) || isdigit(c) || is_special_subsequent(c);
}

static Token get_token_dotty(Parser *p)
{
    int c = fgetc(p->in);
    if (c == '.' && (c = fgetc(p->in)) == '.') {
        return TOK_IDENT("...");
    }
    ungetc(c, p->in);
    return TOK_DOT;
}

static Token get_token_ident(Parser *p)
{
    char buf[BUFSIZ], *s = buf, *end = s + sizeof(buf);
    int c = fgetc(p->in);

    if (!is_initial(c))
        unexpected("identifier", "'%c' as initial", c);
    *s++ = c;
    for (;;) {
        c = fgetc(p->in);
        if (!is_subsequent(c))
            break;
        *s++ = c;
        if (s == end)
            unexpected("identifier", "too long");
    }
    ungetc(c, p->in);
    *s = '\0';
    return TOK_IDENT(buf);
}

static Token get_token_after_sign(Parser *p, int csign)
{
    int c = fgetc(p->in);
    int dig = isdigit(c);
    ungetc(c, p->in);
    if (dig) {
        int sign = csign == '-' ? -1 : 1;
        return get_token_int(p, sign);
    }
    char ident[] = { csign, '\0' };
    return TOK_IDENT(ident);
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
        return get_token_dotty(p);
    case '"':
        return get_token_string(p);
    case EOF:
        return TOK_EOF;
    default:
        break;
    }
    if (c == '-' || c == '+')
        return get_token_after_sign(p, c);
    if (isdigit(c)) {
        ungetc(c, p->in);
        return get_token_int(p, 1);
    }
    if (isalpha(c) || is_special_initial(c)) {
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
        unexpected("')'", "'%s'", token_stringify(t));
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
        unexpected("')'", "'%s'", token_stringify(t));
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
        unexpected("expression", "')'");
    case TTYPE_DOT:
        unexpected("expression", "'.'");
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

static long length(Value list)
{
    long l = 0;
    while (list != Qnil) {
        l++;
        list = cdr(list);
    }
    return l;
}

Value funcall(Value func, Value vargs)
{
    static const long ARG_MAX = 7;

    long n = FUNCTION(func)->arity;
    if (n > ARG_MAX)
        error("arguments too long: max is %ld but got %ld", ARG_MAX, n);
    long l = length(vargs);
    if (l != n)
        error("wrong number of arguments: expected %ld but got %ld", n, l);

    Value a[ARG_MAX];
    Value v = vargs;
    for (long i = 0; i < n; i++) {
        a[i] = car(v);
        v = cdr(v);
    }
    CFunc f = FUNCTION(func)->cfunc;
    switch (n) {
    case 0:
        return (*f)();
    case 1:
        return (*f)(a[0]);
    case 2:
        return (*f)(a[0], a[1]);
    case 3:
        return (*f)(a[0], a[1], a[2]);
    case 4:
        return (*f)(a[0], a[1], a[2], a[3]);
    case 5:
        return (*f)(a[0], a[1], a[2], a[3], a[4]);
    case 6:
        return (*f)(a[0], a[1], a[2], a[3], a[4], a[5]);
    case 7:
        return (*f)(a[0], a[1], a[2], a[3], a[4], a[5], a[6]);
    default:
        UNREACHABLE();
    }
}

static void expect_type(Type expected, Value v, const char *header)
{
    Type t = value_typeof(v);
    if (t == expected)
        return;
    const char *delim;
    if (header == NULL)
        header = delim = "";
    else
        delim = ": ";
    error("%s%stype error: expected %s but got %s",
          header, delim, TYPE_NAMES[expected], TYPE_NAMES[t]);
}

#define expect_type_pair(t, x, y, name) \
    expect_type(t, x, name); \
    expect_type(t, y, name);

static Value builtin_add(Value x, Value y)
{
    expect_type_pair(TYPE_INT, x, y, "+");

    intptr_t ix = value_to_int(x);
    intptr_t iy = value_to_int(y);
    return value_of_int(ix + iy);
}

static Value builtin_sub(Value x, Value y)
{
    expect_type_pair(TYPE_INT, x, y, "-");

    intptr_t ix = value_to_int(x);
    intptr_t iy = value_to_int(y);
    return value_of_int(ix - iy);
}

static Value builtin_mul(Value x, Value y)
{
    expect_type_pair(TYPE_INT, x, y, "*");

    intptr_t ix = value_to_int(x);
    intptr_t iy = value_to_int(y);
    return value_of_int(ix * iy);
}

static Value builtin_div(Value x, Value y)
{
    expect_type_pair(TYPE_INT, x, y, "/");

    intptr_t ix = value_to_int(x);
    intptr_t iy = value_to_int(y);
    return value_of_int(ix / iy);
}

typedef Value (*FuncMapper)(Value);

static Value map(FuncMapper f, Value l)
{
    Value mapped, next, last = Qnil;
    for (; l != Qnil; l = cdr(l), last = next) {
        next = cons(f(car(l)), Qnil);
        if (last == Qnil)
            mapped = next;
        else
            PAIR(last)->cdr = next;
    }
    return mapped;
}

static Value environment = Qnil; // alist of ('ident . <value>)

// keys may duplicated
static Value alist_prepend(Value *l, Value key, Value val)
{
    Value entry = cons(key, val);
    Value pair = cons(entry, *l);
    *l = pair;
    return *l;
}

static Value put_env(const char *name, Value val)
{
    alist_prepend(&environment, value_of_symbol(name), val);
    return val;
}

static Value define_function(const char *name, CFunc cfunc, long arity)
{
    return put_env(name, value_of_func(cfunc, arity));
}

static Value alist_find(Value l, Value vkey)
{
    if (!value_is_symbol(vkey))
        return Qnil;
    Symbol key = value_to_symbol(vkey);
    Value p;
    for (p = l; p != Qnil; p = cdr(p)) {
        Value entry = car(p);
        if (!value_is_pair(entry))
            continue;
        Value target = car(entry);
        if (!value_is_symbol(target))
            continue;
        Symbol sym = value_to_symbol(target);
        if (sym == key)
            return cdr(entry);
   }
    return Qnil;
}

static Value lookup_func(Value name)
{
    Value f = alist_find(environment, name);
    if (f == Qnil)
        error("unknown function name '%s'", unintern(name));
    return f;
}

Value eval_string(const char *s)
{
    return eval(parse_expr_string(s));
}

static bool eval_init(void)
{
    define_function("+", builtin_add, 2);
    define_function("-", builtin_sub, 2);
    define_function("*", builtin_mul, 2);
    define_function("/", builtin_div, 2);
    return true;
}

static Value eval_func(Value list)
{
    Value name = car(list);
    if (!value_is_symbol(name))
        unexpected("symbol (applicable)", "%s", stringify(name));

    Value f = lookup_func(name);
    Value args = map(eval, cdr(list));
    return funcall(f, args);
}

Value eval(Value v)
{
    static bool initialized = false;
    if (!initialized)
        initialized = eval_init();

    if (value_is_atom(v)) // int, symbol, string
        return v;
    return eval_func(v);
}

Value load(FILE *in)
{
    Value last = Qnil;
    for (Value v = parse(in); !value_is_nil(v); v = cdr(v))
        last = eval(car(v));
    return last;
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

Value parse_expr_string(const char *in)
{
    FILE *f = fmemopen((char *)in, strlen(in), "r");
    Parser *p = parser_new(f);
    Value v = parse_expr(p);
    free(p);
    fclose(f);
    return v;
}

Value parse_string(const char *in)
{
    FILE *f = fmemopen((char *)in, strlen(in), "r");
    Value v = parse(f);
    fclose(f);
    return v;
}
