#include <ctype.h>
#include <setjmp.h>
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
#define parse_error(exp, act, ...) \
    runtime_error("while parsing: expected %s but got " act, exp __VA_OPT__(,) __VA_ARGS__)

static jmp_buf jmp_runtime_error;
static char errmsg[BUFSIZ];

ATTR_NORETURN
static void runtime_error(const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    vsnprintf(errmsg, sizeof(errmsg), fmt, ap);
    longjmp(jmp_runtime_error, Qundef);
}

const char *error_message(void)
{
    return errmsg;
}

typedef enum {
// immediate
    TYPE_BOOL,
    TYPE_INT,
    TYPE_SYMBOL,
    TYPE_UNDEF,
// boxed (tagged)
    TYPE_PAIR,
    TYPE_STR,
    TYPE_CFUNC,
    TYPE_SPECIAL,
    TYPE_CLOSURE,
} Type;

static const char *TYPE_NAMES[] = {
    [TYPE_BOOL] = "boolean",
    [TYPE_INT] = "integer",
    [TYPE_SYMBOL] = "symbol",
    [TYPE_UNDEF] = "undef",
    [TYPE_PAIR] = "pair",
    [TYPE_STR] = "internal string",
    [TYPE_CFUNC] = "C function",
    [TYPE_SPECIAL] = "special form",
    [TYPE_CLOSURE] = "closure",
};

typedef enum {
    TAG_PAIR,
    TAG_STR,
    TAG_CFUNC,
    TAG_SPECIAL, // almost a Function
    TAG_CLOSURE,
} ValueTag;

typedef struct Pair {
    ValueTag tag; // common
    Value car, cdr;
} Pair;

typedef struct {
    ValueTag tag;
    const char *body;
} String;

typedef struct {
    ValueTag tag;
    long arity;
    union {
        CFunc cfunc;
        Value closure; // '(env (params . body))
    };
} Function;

#define VALUE_TAG(v) (*(ValueTag*)(v))
#define PAIR(v) ((Pair *) v)
#define STRING(v) ((String *) v)
#define FUNCTION(v) ((Function *) v)

// singletons
static const Pair PAIR_NIL = { .tag = TAG_PAIR, .car = 0, .cdr = 0 };
// Value (uintptr_t):
//   0b....000 Pointer
//   0b......1 Integer
//   0b...1110 symbol
//   0b0..0010 #f
//   0b0..0100 #t
//   0b0..0110 <undef>
static const uintptr_t FLAG_NBIT = 4U;
static const uintptr_t MASK_IMMEDIATE = 0b1111U;
static const uintptr_t FLAG_SYMBOL    = 0b1110U;
const Value Qnil = (Value) &PAIR_NIL;
const Value Qfalse = 0b0010U;
const Value Qtrue  = 0b0100U;
const Value Qundef = 0b0110U; // may be an error or something

static const long FUNCARG_MAX = 7;

// runtime-locals (aka global variables)

static Value toplevel_environment = Qnil; // alist of ('ident . <value>)
static Value symbol_names = Qnil; // ('ident 'ident2 ...)
static Value SYM_ELSE = Qundef; // used in cond

// value_is_*: type checks

static inline uintptr_t flags(Value v)
{
    return v & MASK_IMMEDIATE;
}

inline bool value_is_int(Value v)
{
    return v & 1U;
}

inline bool value_is_symbol(Value v)
{
    return flags(v) == FLAG_SYMBOL;
}

static inline bool is_immediate(Value v)
{
    return !!flags(v);
}

static inline bool tagged_value_is(Value v, ValueTag expected)
{
    return !is_immediate(v) && VALUE_TAG(v) == expected;
}

inline bool value_is_cfunc(Value v)
{
    return tagged_value_is(v, TAG_CFUNC);
}

inline bool value_is_closure(Value v)
{
    return tagged_value_is(v, TAG_CLOSURE);
}

inline bool value_is_pair(Value v)
{
    return tagged_value_is(v, TAG_PAIR);
}

inline bool value_is_atom(Value v)
{
    return !value_is_pair(v);
}

inline bool value_is_nil(Value v)
{
    return v == Qnil;
}

static inline Type value_typeof(Value v)
{
    if (is_immediate(v)) {
        if (value_is_int(v))
            return TYPE_INT;
        if (value_is_symbol(v))
            return TYPE_SYMBOL;
        if (v == Qtrue || v == Qfalse)
            return TYPE_BOOL;
        return TYPE_UNDEF;
    }
    switch (VALUE_TAG(v)) {
    case TAG_STR:
        error("got internal string: %s", value_to_string(v));
    case TAG_PAIR:
        return TYPE_PAIR;
    case TAG_CFUNC:
        return TYPE_CFUNC;
    case TAG_SPECIAL:
        return TYPE_SPECIAL;
    case TAG_CLOSURE:
        return TYPE_CLOSURE;
    }
    UNREACHABLE();
}

// value_to_*: convert internal data to external plain C

inline int64_t value_to_int(Value v)
{
    return (int64_t) v >> 1U;
}

inline Symbol value_to_symbol(Value v)
{
    return (Symbol) (v >> FLAG_NBIT);
}

static Symbol intern(const char *s);
static const char *unintern(Symbol sym);

inline const char *value_to_string(Value v)
{
    if (value_is_symbol(v))
        return unintern(value_to_symbol(v));
    return STRING(v)->body;
}

// value_of_*: convert external plain C data to internal

inline Value value_of_int(int64_t i)
{
    return (Value) i << 1U | 1U;
}

inline Value value_of_symbol(const char *s)
{
    Symbol sym = intern(s);
    return (Value) (sym << FLAG_NBIT | FLAG_SYMBOL);
}

static inline void *tagged_new(size_t size, ValueTag t)
{
    void *p = xmalloc(size);
    VALUE_TAG(p) = t;
    return p;
}

static inline Value value_of_string(const char *s)
{
    String *str = tagged_new(sizeof(String), TAG_STR);
    str->body = xstrdup(s);
    return (Value) str;
}

static inline Value value_of_cfunc(CFunc cfunc, long arity)
{
    Function *f = tagged_new(sizeof(Function), TAG_CFUNC);
    f->cfunc = cfunc;
    f->arity = arity;
    return (Value) f;
}

static inline Value value_of_special(CFunc cfunc, long arity)
{
    arity += (arity == -1) ? -1 : 1; // for *env
    Value sp = value_of_cfunc(cfunc, arity);
    FUNCTION(sp)->tag = TAG_SPECIAL;
    return sp;
}

static inline Value value_of_closure(Value env, Value params, Value body)
{
    Function *f = tagged_new(sizeof(Function), TAG_CLOSURE);
    f->arity = length(params);
    f->closure = cons(env, cons(params, body));
    return (Value) f;
}

// `cons` is well-known name than "value_of_pair"
inline Value cons(Value car, Value cdr)
{
    Pair *p = tagged_new(sizeof(Pair), TAG_PAIR);
    p->car = car;
    p->cdr = cdr;
    return (Value) p;
}

// utilities for errors

static void expect_type(const char *header, Type expected, Value v)
{
    Type t = value_typeof(v);
    if (t == expected)
        return;
    runtime_error("type error in %s: expected %s but got %s",
                  header, TYPE_NAMES[expected], TYPE_NAMES[t]);
}
#define expect_type_twin(h, t, x, y) expect_type(h, t, x), expect_type(h, t, y)

// for parsing

static Value append(Value l, Value elem)
{
    Value p = cons(elem, Qnil);
    if (l == Qnil)
        return p;
    PAIR(l)->cdr = p;
    return p;
}

inline Value list(Value v, ...)
{
    Value l = Qnil, last = l;
    va_list ap;
    va_start(ap, v);
    for (; v != Qundef; v = va_arg(ap, Value)) {
        last = append(last, v);
        if (l == Qnil)
            l = last;
    }
    return l;
}

typedef enum {
    TTYPE_LPAREN,
    TTYPE_RPAREN,
    TTYPE_INT,
    TTYPE_DOT,
    TTYPE_IDENT,
    TTYPE_CONST,
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
#define TOK_IDENT(s) TOK_V(IDENT, value_of_symbol(s))
#define TOK_CONST(c) TOK_V(CONST, c)

typedef struct {
    FILE *in;
    Token prev_token;
} Parser;

static Token get_token_int(Parser *p, int sign)
{
    int64_t i;
    int n = fscanf(p->in, "%ld", &i);
    if (n != 1)
        parse_error("integer", "invalid string");
    return TOK_INT(sign * i);
}

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
    if (name == NULL) // fatal; known symbol should have name
        error("symbol %lu not found", sym);
    return name;
}

static inline bool is_initial(int c)
{
    return isalpha(c) ||
        c == '*' || c == '/' || c == '<' || c == '=' || c == '>' || c == '_';
}

static inline bool is_subsequent(int c)
{
    return isalpha(c) || isdigit(c) ||
        c == '*' || c == '-' || c == '.' || c == '!' || c == '=';
}

static Token get_token_ident(Parser *p)
{
    char buf[BUFSIZ], *s = buf, *end = s + sizeof(buf);
    int c = fgetc(p->in);

    *s++ = c;
    for (;;) {
        c = fgetc(p->in);
        if (!is_subsequent(c))
            break;
        *s++ = c;
        if (s == end)
            parse_error("identifier", "too long");
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
        return TOK_DOT;
    case '#':
        c = fgetc(p->in);
        if (c == 't')
            return TOK_CONST(Qtrue);
        if (c == 'f')
            return TOK_CONST(Qfalse);
        parse_error("constants", "#%c", c);
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
    if (is_initial(c)) {
        ungetc(c, p->in);
        return get_token_ident(p);
    }
    parse_error("valid char", "'%c'", c);
}

static void unget_token(Parser *p, Token t)
{
    p->prev_token = t;
}

static inline bool got_eof(Parser *p)
{
    return feof(p->in);
}

inline Value car(Value v)
{
    return PAIR(v)->car;
}

inline Value cdr(Value v)
{
    return PAIR(v)->cdr;
}

#define DEF_CXR(x, y) Value c##x##y##r(Value v) { return c##x##r(c##y##r(v)); }
#define DEF_CXR1(x) DEF_CXR(a, x) DEF_CXR(d, x)
#define DEF_CXR2(x) DEF_CXR1(a ## x) DEF_CXR1(d ## x)
#define DEF_CXR3(x) DEF_CXR2(a ## x) DEF_CXR2(d ## x)
#define DEF_CXR4(x) DEF_CXR3(a ## x) DEF_CXR3(d ## x)
#define DEF_CXRS() DEF_CXR2() DEF_CXR3() DEF_CXR4()

DEF_CXRS()

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
    case TTYPE_CONST:
        return t.value == Qtrue ? "#t" : "#f";
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
        parse_error("')'", "'%s'", token_stringify(t));
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
        parse_error("')'", "'%s'", token_stringify(t));
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
        parse_error("expression", "')'");
    case TTYPE_DOT:
        parse_error("expression", "'.'");
    case TTYPE_INT:
    case TTYPE_CONST:
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

long length(Value list)
{
    long l = 0;
    for (; list != Qnil; list = cdr(list))
        l++;
    return l;
}

static void expect_arity_range(const char *func, long min, long max, long actual)
{
    if ((min == -1 && actual <= max) ||
        (max == -1 && min <= actual) ||
        (min <= actual && actual <= max))
        return;
    runtime_error("%s: wrong number of arguments: expected %ld..%ld but got %ld",
                  func, min, max, actual);
}

static void expect_arity(long arity, long n)
{
    if (arity < 0 || arity == n)
        return;
    runtime_error("wrong number of arguments: expected %ld but got %ld",
                  arity, n);
}

static void scan_args(Value ary[FUNCARG_MAX], long arity, Value args)
{
    long i;
    Value a = args;
    for (i = 0; i < arity; i++) {
        if (a == Qnil)
            break;
        ary[i] = car(a);
        a = cdr(a);
    }
    i += length(a);
    expect_arity(arity, i);
}

static Value apply_cfunc(Value *env, Value func, Value vargs)
{
    Value a[FUNCARG_MAX];
    long n = FUNCTION(func)->arity;
    CFunc f = FUNCTION(func)->cfunc;

    scan_args(a, n, vargs);

    switch (n) {
    case -2:
        return (*f)(env, cdr(vargs)); // special form
    case -1:
        return (*f)(vargs); // non-special
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

static Value ieval(Value *env, Value v); // internal

static Value eval_body(Value *env, Value body)
{
    Value last = Qnil;
    for (Value b = body; b != Qnil; b = cdr(b))
        last = ieval(env, car(b));
    return last;
}

static Value apply_closure(ATTR_UNUSED Value *env, Value func, Value args)
{
    long arity = FUNCTION(func)->arity;
    expect_arity(arity, length(args));

    Value closure = FUNCTION(func)->closure;
    Value clenv = car(closure), params = cadr(closure), body = cddr(closure);
    if (arity == -1)
        runtime_error("(apply): variadic arguments not supported yet");
    while (args != Qnil) {
        clenv = cons(cons(car(params), car(args)), clenv);
        args = cdr(args);
        params = cdr(params);
    }
    return eval_body(&clenv, body);
}

typedef Value (*MapFunc)(Value *common, Value v);
static Value map2(MapFunc f, Value *common, Value l)
{
    Value mapped = Qnil, last = Qnil;
    for (; l != Qnil; l = cdr(l)) {
        last = append(last, f(common, car(l)));
        if (mapped == Qnil)
            mapped = last;
    }
    return mapped;
}

static Value apply(Value *env, Value func, Value args)
{
    if (is_immediate(func))
        goto unapplicative;
    switch (VALUE_TAG(func)) {
    case TAG_SPECIAL:
        args = cons((Value) env, args);
        return apply_cfunc(env, func, args);
    case TAG_CFUNC:
        args = map2(ieval, env, args);
        return apply_cfunc(env, func, args);
    case TAG_CLOSURE:
        args = map2(ieval, env, args);
        return apply_closure(env, func, args);
    default:
        break;
    }
 unapplicative:
    runtime_error("type error in (eval): expected applicative but got %s",
                  TYPE_NAMES[value_typeof(func)]);
}

static Value alist_find(Value l, Value key)
{
    for (Value p = l; p != Qnil; p = cdr(p)) {
        Value entry = car(p);
        if (value_is_pair(entry) && car(entry) == key)
            return entry;
    }
    return Qnil;
}

static Value alist_prepend(Value list, Value key, Value val)
{
    return cons(cons(key, val), list);
}

static void expect_valid_arity(long expected_max, long actual)
{
    if (actual <= expected_max)
        return;
    error("arity too large: expected ..%ld but got %ld",
          expected_max, actual);
}

static Value define_special(Value *env, const char *name, CFunc cfunc, long arity)
{
    expect_valid_arity(FUNCARG_MAX - 1, arity);
    *env = alist_prepend(*env, value_of_symbol(name), value_of_special(cfunc, arity));
    return Qnil;
}

static Value define_function(Value *env, const char *name, CFunc cfunc, long arity)
{
    expect_valid_arity(FUNCARG_MAX, arity);
    *env = alist_prepend(*env, value_of_symbol(name), value_of_cfunc(cfunc, arity));
    return Qnil;
}

static Value lookup(Value env, Value name)
{
    Value found = alist_find(env, name);
    if (found == Qnil)
        runtime_error("unbound variable: %s", value_to_string(name));
    return cdr(found);
}

Value eval_string(const char *in)
{
    FILE *f = fmemopen((char *)in, strlen(in), "r");
    Value v = load(f);
    fclose(f);
    return v;
}

static Value ieval(Value *env, Value v)
{
    if (value_is_symbol(v))
        return lookup(*env, v);
    if (v == Qnil || is_immediate(v))
        return v;
    Value func = ieval(env, car(v));
    return apply(env, func, cdr(v));
}

Value eval(Value v)
{
    return ieval(&toplevel_environment, v);
}

Value load(FILE *in)
{
    Value env = toplevel_environment;
    if (setjmp(jmp_runtime_error) != 0)
        return Qundef;
    return eval_body(&env, parse(in));
}

static void fdisplay(FILE* f, Value v);

static void display_list(FILE *f, Value v)
{
    for (;;) {
        Pair *p = PAIR(v);
        fdisplay(f, p->car);
        v = p->cdr;
        if (v == Qnil)
            break;
        fprintf(f, " ");
        if (value_is_atom(v)) {
            fprintf(f, ". ");
            fdisplay(f, v);
            break;
        }
    }
}

static void fdisplay(FILE* f, Value v)
{
    switch (value_typeof(v)) {
    case TYPE_BOOL:
        fprintf(f, "%s", v == Qtrue ? "#t" : "#f");
        break;
    case TYPE_INT:
        fprintf(f, "%ld", value_to_int(v));
        break;
    case TYPE_SYMBOL:
        fprintf(f, "'%s", value_to_string(v));
        break;
    case TYPE_PAIR:
        fprintf(f, "(");
        if (v != Qnil)
            display_list(f, v);
        fprintf(f, ")");
        break;
    case TYPE_STR:
        error("got internal string: %s", value_to_string(v));
    case TYPE_CFUNC:
        fprintf(f, "<function>");
        break;
    case TYPE_SPECIAL:
        fprintf(f, "<special>");
        break;
    case TYPE_CLOSURE:
        fprintf(f, "<closure>");
        break;
    case TYPE_UNDEF:
        fprintf(f, "<undef>");
        break;
    }
}

void display(Value v)
{
    fdisplay(stdout, v);
}

char *stringify(Value v)
{
    char *s;
    size_t size;
    FILE *stream = open_memstream(&s, &size);
    if (stream == NULL)
        return NULL;
    fdisplay(stream, v);
    fclose(stream);
    return s;
}

Value reverse(Value v)
{
    Value l = Qnil;
    for (; v != Qnil; v = cdr(v))
        l = cons(car(v), l);
    return l;
}

Value parse(FILE *in)
{
    Parser *p = parser_new(in);
    Value v = Qnil;
    for (;;) {
        Value expr = parse_expr(p);
        if (expr == Qnil && got_eof(p))
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

static int64_t value_get_int(const char *header, Value v)
{
    expect_type(header, TYPE_INT, v);
    return value_to_int(v);
}

static Value builtin_add(Value args)
{
    int64_t y = 0;
    for (Value l = args; l != Qnil; l = cdr(l))
        y += value_get_int("+", car(l));
    return value_of_int(y);
}

static Value builtin_sub(Value args)
{
    expect_arity_range("-", 1, -1, length(args));

    Value rest = cdr(args);
    int64_t y = 0;
    if (rest == Qnil)
        rest = args;
    else {
        y = value_get_int("-", car(args));
    }
    for (Value l = rest; l != Qnil; l = cdr(l))
        y -= value_get_int("-", car(l));
    return value_of_int(y);
}

static Value builtin_mul(Value args)
{
    int64_t y = 1;
    for (Value l = args; l != Qnil; l = cdr(l))
        y *= value_get_int("*", car(l));
    return value_of_int(y);
}

static Value builtin_div(Value args)
{
    expect_arity_range("/", 1, -1, length(args));

    Value rest = cdr(args);
    int64_t y = 1;
    if (rest == Qnil)
        rest = args;
    else
        y = value_get_int("/", car(args));
    for (Value l = rest; l != Qnil; l = cdr(l)) {
        int64_t x = value_get_int("/", car(l));
        if (x == 0)
            runtime_error("/: divided by zero");
        y /= x;
    }
    return value_of_int(y);
}

static Value builtin_numeq(Value args)
{
    expect_arity_range("=", 2, -1, length(args));

    int64_t x = value_get_int("=", car(args));
    while ((args = cdr(args)) != Qnil) {
        int64_t y = value_get_int("=", car(args));
        if (x != y)
            return Qfalse;
    }
    return Qtrue;
}

static Value builtin_lt(Value args)
{
    expect_arity_range("<", 2, -1, length(args));

    int64_t x = value_get_int("<", car(args));
    while ((args = cdr(args)) != Qnil) {
        int64_t y = value_get_int("<", car(args));
        if (x >= y)
            return Qfalse;
        x = y;
    }
    return Qtrue;
}

static Value builtin_gt(Value args)
{
    expect_arity_range(">", 2, -1, length(args));

    int64_t x = value_get_int(">", car(args));
    while ((args = cdr(args)) != Qnil) {
        int64_t y = value_get_int(">", car(args));
        if (x <= y)
            return Qfalse;
        x = y;
    }
    return Qtrue;
}

static Value builtin_le(Value args)
{
    expect_arity_range("<=", 2, -1, length(args));

    int64_t x = value_get_int("<=", car(args));
    while ((args = cdr(args)) != Qnil) {
        int64_t y = value_get_int("<=", car(args));
        if (x > y)
            return Qfalse;
        x = y;
    }
    return Qtrue;
}

static Value builtin_ge(Value args)
{
    expect_arity_range(">=", 2, -1, length(args));

    int64_t x = value_get_int(">=", car(args));
    while ((args = cdr(args)) != Qnil) {
        int64_t y = value_get_int(">=", car(args));
        if (x < y)
            return Qfalse;
        x = y;
    }
    return Qtrue;
}

static Value builtin_if(Value *env, Value args)
{
    expect_arity_range("if", 2, 3, length(args));

    Value cond = car(args), then = cadr(args);
    if (ieval(env, cond) != Qfalse)
        return ieval(env, then);
    Value els = cddr(args);
    if (els == Qnil)
        return Qnil;
    return ieval(env, car(els));
}

static Value builtin_define(Value *env, Value ident, Value expr)
{
    expect_type("define", TYPE_SYMBOL, ident);

    Value found = Qnil;
    if (*env == toplevel_environment) // set?
        found = alist_find(*env, ident);
    if (found == Qnil) // prepend
        found = *env = alist_prepend(*env, ident, Qundef);
    PAIR(car(found))->cdr = ieval(env, expr); // set!
    return Qnil;
}

static Value builtin_set(Value *env, Value ident, Value expr)
{
    expect_type("set!", TYPE_SYMBOL, ident);

    Value found = alist_find(*env, ident);
    if (found == Qnil)
        runtime_error("set!: unbound variable: %s", value_to_string(ident));
    PAIR(found)->cdr = ieval(env, expr);
    return Qnil;
}

static Value builtin_let(Value *env, Value args)
{
    Value bindings = car(args);
    Value body = cdr(args);
    expect_type_twin("let", TYPE_PAIR, bindings, body);
    Value letenv = *env;
    for (; bindings != Qnil; bindings = cdr(bindings)) {
        Value b = car(bindings);
        if (b == Qnil)
            continue;
        expect_type("let", TYPE_PAIR, b);
        Value ident = car(b), expr = cadr(b);
        expect_type("let", TYPE_SYMBOL, ident);
        letenv = cons(cons(ident, ieval(env, expr)), letenv);
    }
    if (body == Qnil)
        runtime_error("let: one or more expressions needed in body");
    return eval_body(&letenv, body);
}

static Value builtin_lambda(Value *env, Value args)
{
    Value params = car(args);
    Value body = cdr(args);
    expect_type_twin("let", TYPE_PAIR, params, body);
    if (body == Qnil)
        runtime_error("let: one or more expressions needed in body");
    return value_of_closure(*env, params, body);
}

static Value builtin_list(Value args)
{
    return args;
}

static Value builtin_display(Value obj)
{
    display(obj);
    return obj;
}

static Value builtin_newline(void)
{
    puts("");
    return Qnil;
}

static Value builtin_begin(Value *env, Value body)
{
    return eval_body(env, body);
}

static Value builtin_cond(Value *env, Value clauses)
{
    expect_arity_range("cond", 1, -1, length(clauses));

    for (; clauses != Qnil; clauses = cdr(clauses)) {
        Value clause = car(clauses);
        expect_type("cond", TYPE_PAIR, clause);
        Value test = car(clause);
        Value exprs = cdr(clause);
        if (test == SYM_ELSE)
            return exprs == Qnil ? Qtrue : eval_body(env, exprs);
        Value t = ieval(env, test);
        if (t != Qfalse)
            return exprs == Qnil ? t : eval_body(env, exprs);
    }
    return Qnil;
}

ATTR_CTOR
static void initialize(void)
{
    SYM_ELSE = value_of_symbol("else");

    Value *e = &toplevel_environment;
    define_special(e, "if", builtin_if, -1);
    define_special(e, "define", builtin_define, 2);
    define_special(e, "set!", builtin_set, 2);
    define_special(e, "let", builtin_let, -1);
    define_special(e, "let*", builtin_let, -1); // alias
    define_special(e, "lambda", builtin_lambda, -1);
    define_special(e, "begin", builtin_begin, -1);
    define_special(e, "cond", builtin_cond, -1);

    define_function(e, "+", builtin_add, -1);
    define_function(e, "-", builtin_sub, -1);
    define_function(e, "*", builtin_mul, -1);
    define_function(e, "/", builtin_div, -1);
    define_function(e, "=", builtin_numeq, -1);
    define_function(e, "<", builtin_lt, -1);
    define_function(e, ">", builtin_gt, -1);
    define_function(e, "<=", builtin_le, -1);
    define_function(e, ">=", builtin_ge, -1);

    define_function(e, "car", car, 1);
    define_function(e, "cdr", cdr, 1);
    define_function(e, "cons", cons, 2);
    define_function(e, "list", builtin_list, -1);
    define_function(e, "reverse", reverse, 1);
    define_function(e, "display", builtin_display, 1);
    define_function(e, "newline", builtin_newline, 0);
}
