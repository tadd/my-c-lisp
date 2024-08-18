#include <ctype.h>
#include <inttypes.h>
#include <math.h>
#include <setjmp.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include <libgen.h>
#include <limits.h>
#include <unistd.h>

#include "lisp.h"
#include "utils.h"

//
// Errors
//

#define error(fmt, ...) \
    error("%s:%d of %s: " fmt, __FILE__, __LINE__, __func__ __VA_OPT__(,) __VA_ARGS__)

static jmp_buf jmp_runtime_error, jmp_parse_error;
static char errmsg[BUFSIZ];

ATTR_NORETURN
static void runtime_error(const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    vsnprintf(errmsg, sizeof(errmsg), fmt, ap);
    va_end(ap);
    longjmp(jmp_runtime_error, 1);
}

const char *error_message(void)
{
    return errmsg;
}

//
// Types
//

static const char *TYPE_NAMES[] = {
    [TYPE_BOOL] = "boolean",
    [TYPE_INT] = "integer",
    [TYPE_SYMBOL] = "symbol",
    [TYPE_UNDEF] = "undef",
    [TYPE_PAIR] = "pair",
    [TYPE_STR] = "string",
    [TYPE_CFUNC] = "C function",
    [TYPE_SPECIAL] = "special form",
    [TYPE_CLOSURE] = "closure",
    [TYPE_CONTINUATION] = "continuation",
};

typedef enum { // has the same values as Type
    TAG_PAIR    = TYPE_PAIR,
    TAG_STR     = TYPE_STR,
    TAG_CFUNC   = TYPE_CFUNC,
    TAG_SPECIAL = TYPE_SPECIAL, // almost a C Function
    TAG_CLOSURE = TYPE_CLOSURE,
    TAG_CONTINUATION = TYPE_CONTINUATION,
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
    int64_t arity;
} Function;

typedef struct {
    Function func;
    cfunc_t cfunc;
} CFunc;

typedef struct {
    Function func;
    Value env;
    Value params;
    Value body;
} Closure;

typedef struct {
    Function func;
    volatile void *sp;
    void *shelter;
    size_t shelter_len;
    jmp_buf state;
    Value retval;
} Continuation;

#define VALUE_TAG(v) (*(ValueTag*)(v))
#define PAIR(v) ((Pair *) v)
#define STRING(v) ((String *) v)
#define FUNCTION(v) ((Function *) v)
#define CFUNC(v) ((CFunc *) v)
#define CLOSURE(v) ((Closure *) v)
#define CONTINUATION(v) ((Continuation *) v)
#define OF_BOOL(v) ((v) ? Qtrue : Qfalse)

// singletons
static const Pair PAIR_NIL = { .tag = TAG_PAIR, .car = 0, .cdr = 0 };
// Value (uintptr_t):
//   0b....000 Pointer (Unchangeable pattern!)
//   0b......1 Integer
//   0b...1110 Symbol
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

static const int64_t CFUNCARG_MAX = 7;

//
// Runtime-locals (aka global variables)
//

static Value toplevel_environment = Qnil; // alist of ('symbol . <value>)
static Value symbol_names = Qnil; // ("name0" "name1" ...)
static Value SYM_ELSE = Qundef; // used in cond
static const volatile void *stack_base = NULL;
#define INIT_STACK() void *basis; stack_base = &basis
static const char *load_basedir = NULL;

//
// value_is_*: Type Checks
//

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
    return v & 0b111U; // for 64 bit machine
}

static inline bool value_tag_is(Value v, ValueTag expected)
{
    return !is_immediate(v) && VALUE_TAG(v) == expected;
}

inline bool value_is_string(Value v)
{
    return value_tag_is(v, TAG_STR);
}

inline bool value_is_cfunc(Value v)
{
    return value_tag_is(v, TAG_CFUNC);
}

inline bool value_is_closure(Value v)
{
    return value_tag_is(v, TAG_CLOSURE);
}

inline bool value_is_pair(Value v)
{
    return value_tag_is(v, TAG_PAIR);
}

inline bool value_is_atom(Value v)
{
    return !value_is_pair(v);
}

inline bool value_is_nil(Value v)
{
    return v == Qnil;
}

Type value_type_of(Value v)
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
    ValueTag t = VALUE_TAG(v);
    switch (t) {
    case TAG_STR:
    case TAG_PAIR:
    case TAG_CFUNC:
    case TAG_SPECIAL:
    case TAG_CLOSURE:
    case TAG_CONTINUATION:
        return (Type) t;
    }
    UNREACHABLE();
}

inline const char *value_type_to_string(Type t)
{
    return TYPE_NAMES[t];
}

// value_to_*: Convert internal data to external plain C

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

// value_of_*: Convert external plain C data to internal

inline Value value_of_int(int64_t i)
{
    return (Value) i << 1U | 1U;
}

inline Value value_of_symbol(const char *s)
{
    Symbol sym = intern(s);
    return (Value) (sym << FLAG_NBIT | FLAG_SYMBOL);
}

static void *obj_new(size_t size, ValueTag t)
{
    void *p = xmalloc(size);
    VALUE_TAG(p) = t;
    return p;
}

Value value_of_string(const char *s)
{
    String *str = obj_new(sizeof(String), TAG_STR);
    str->body = xstrdup(s);
    return (Value) str;
}

static Value value_of_cfunc(cfunc_t cfunc, int64_t arity)
{
    CFunc *f = obj_new(sizeof(CFunc), TAG_CFUNC);
    f->func.arity = arity;
    f->cfunc = cfunc;
    return (Value) f;
}

static Value value_of_special(cfunc_t cfunc, int64_t arity)
{
    arity += (arity == -1) ? -1 : 1; // for *env
    Value sp = value_of_cfunc(cfunc, arity);
    VALUE_TAG(sp) = TAG_SPECIAL;
    return sp;
}

static Value value_of_closure(Value env, Value params, Value body)
{
    Closure *f = obj_new(sizeof(Closure), TAG_CLOSURE);
    f->func.arity = (value_type_of(params) == TYPE_PAIR) ? length(params) : -1;
    f->env = env;
    f->params = params;
    f->body = body;
    return (Value) f;
}

// `cons` is well-known name than "value_of_pair"
Value cons(Value car, Value cdr)
{
    Pair *p = obj_new(sizeof(Pair), TAG_PAIR);
    p->car = car;
    p->cdr = cdr;
    return (Value) p;
}

// utilities for errors

static void expect_type(const char *header, Type expected, Value v)
{
    Type t = value_type_of(v);
    if (t == expected)
        return;
    runtime_error("type error in %s: expected %s but got %s",
                  header, value_type_to_string(expected), value_type_to_string(t));
}
#define expect_type_twin(h, t, x, y) expect_type(h, t, x), expect_type(h, t, y)

static void expect_type_or(const char *header, Type e1, Type e2, Value v)
{
    Type t = value_type_of(v);
    if (t == e1 || t == e2)
        return;
    runtime_error("type error in %s: expected %s or %s but got %s",
                  header, value_type_to_string(e1), value_type_to_string(e2),
                  value_type_to_string(t));
}

// Lists: prepare for parsing

// l: last pair
static Value append_at(Value l, Value elem)
{
    Value p = cons(elem, Qnil);
    if (l == Qnil)
        return p;
    PAIR(l)->cdr = p;
    return p;
}

Value list(Value v, ...)
{
    Value l = Qnil, last = l;
    va_list ap;
    va_start(ap, v);
    for (; v != Qundef; v = va_arg(ap, Value)) {
        last = append_at(last, v);
        if (l == Qnil)
            l = last;
    }
    va_end(ap);
    return l;
}

//
// Parse
//

typedef enum {
    TOK_TYPE_LPAREN,
    TOK_TYPE_RPAREN,
    TOK_TYPE_INT,
    TOK_TYPE_DOT,
    TOK_TYPE_STR,
    TOK_TYPE_IDENT,
    TOK_TYPE_CONST,
    TOK_TYPE_EOF
} TokenType;

typedef struct {
    TokenType type;
    Value value;
} Token;

#define TOK(t) { .type = TOK_TYPE_ ## t }
// singletons
static const Token
    TOK_LPAREN = TOK(LPAREN),
    TOK_RPAREN = TOK(RPAREN),
    TOK_DOT = TOK(DOT),
    TOK_EOF = TOK(EOF);
// and ctor
#define TOK_V(t, v) ((Token) { .type = TOK_TYPE_ ## t, .value = v })
#define TOK_INT(i) TOK_V(INT, value_of_int(i))
#define TOK_STR(s) TOK_V(STR, value_of_string(s))
#define TOK_IDENT(s) TOK_V(IDENT, value_of_symbol(s))
#define TOK_CONST(c) TOK_V(CONST, c)

typedef struct {
    FILE *in;
    Token prev_token;
} Parser;

#define parse_error(p, exp, act, ...) do { \
        int64_t line, col; \
        get_line_column(p, &line, &col); \
        memcpy(jmp_runtime_error, jmp_parse_error, sizeof(jmp_buf)); \
        runtime_error("on %"PRId64":%"PRId64": expected %s but got " act, line, col, \
                      exp __VA_OPT__(,) __VA_ARGS__); \
    } while (0)

static void get_line_column(Parser *p, int64_t *line, int64_t *col)
{
    FILE *in = p->in;
    int64_t loc = ftell(in);
    if (loc < 0) {
        *line = *col = 0;
        return;
    }
    rewind(in);
    int64_t nline = 1;
    int64_t last_newline = 0;
    for (int64_t i = 0; i < loc; i++) {
        if (fgetc(in) == '\n') {
            nline++;
            last_newline = i;
        }
    }
    *line = nline;
    *col = loc - last_newline;
}

static Token get_token_int(Parser *p, int sign)
{
    int64_t i;
    int n = fscanf(p->in, "%"SCNd64, &i);
    if (n != 1)
        parse_error(p, "integer", "invalid string");
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
                parse_error(p, "'\\' or '\"' in string literal", "'%c'", c);
        }
        if (pbuf == end)
            parse_error(p, "string literal", "too long: \"%s...\"", pbuf);
        *pbuf++ = c;
    }
    *pbuf = '\0';
    return TOK_STR(buf);
}

static Symbol intern(const char *name)
{
    int64_t i;
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

static const char *name_nth(Value list, int64_t n)
{
    for (int64_t i = 0; i < n; i++) {
        list = cdr(list);
        if (list == Qnil)
            return NULL;
    }
    Value name = car(list);
    return STRING(name)->body;
}

static const char *unintern(Symbol sym)
{
    const char *name = name_nth(symbol_names, (int64_t) sym);
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
        c == '*' || c == '/' || c == '-' || c == '.' || c == '!' || c == '=' ||
        c == '?';
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
            parse_error(p, "identifier", "too long");
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

static void skip_token_atmosphere(Parser *p)
{
    int c;
    for (;;) {
        c = fgetc(p->in);
        if (isspace(c))
            continue; // skip
        if (c == ';') {
            do {
                c = fgetc(p->in);
            } while (c != '\n' && c != EOF);
            continue;
        }
        break;
    }
    ungetc(c, p->in);
}

static Token get_token(Parser *p)
{
    if (p->prev_token.type != TOK_TYPE_EOF)  {
        Token t = p->prev_token;
        p->prev_token = TOK_EOF;
        return t;
    }

    skip_token_atmosphere(p);
    int c = fgetc(p->in);
    switch (c) {
    case '(':
        return TOK_LPAREN;
    case ')':
        return TOK_RPAREN;
    case '.':
        return TOK_DOT;
    case '"':
        return get_token_string(p);
    case '#':
        c = fgetc(p->in);
        if (c == 't')
            return TOK_CONST(Qtrue);
        if (c == 'f')
            return TOK_CONST(Qfalse);
        parse_error(p, "constants", "#%c", c);
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
    parse_error(p, "valid char", "'%c'", c);
}

static void unget_token(Parser *p, Token t)
{
    p->prev_token = t;
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
    case TOK_TYPE_LPAREN:
        return "(";
    case TOK_TYPE_RPAREN:
        return ")";
    case TOK_TYPE_DOT:
        return ".";
    case TOK_TYPE_INT:
        snprintf(buf, sizeof(buf), "%"PRId64, value_to_int(t.value));
        break;
    case TOK_TYPE_IDENT:
        return value_to_string(t.value);
    case TOK_TYPE_STR:
        snprintf(buf, sizeof(buf), "\"%s\"", STRING(t.value)->body);
        break;
    case TOK_TYPE_CONST:
        return t.value == Qtrue ? "#t" : "#f";
    case TOK_TYPE_EOF:
        return "EOF";
    }
    return buf;
}

static Value parse_dotted_pair(Parser *p, Value l, Value last)
{
    if (l == Qnil)
        parse_error(p, "expression", "'.'");
    Value e = parse_expr(p);
    Token t = get_token(p);
    if (t.type != TOK_TYPE_RPAREN)
        parse_error(p, "')'", "'%s'", token_stringify(t));
    PAIR(last)->cdr = e;
    return l;
}

static Value parse_list(Parser *p)
{
    Value l = Qnil, last = Qnil;
    for (;;) {
        Token t = get_token(p);
        if (t.type == TOK_TYPE_RPAREN)
            break;
        if (t.type == TOK_TYPE_EOF)
            parse_error(p, "')'", "'%s'", token_stringify(t));
        if (t.type == TOK_TYPE_DOT)
            return parse_dotted_pair(p, l, last);
        unget_token(p, t);
        Value e = parse_expr(p);
        last = append_at(last, e);
        if (l == Qnil)
            l = last;
    }
    return l;
}

static Value parse_expr(Parser *p)
{
    Token t = get_token(p);
    switch (t.type) {
    case TOK_TYPE_LPAREN:
        return parse_list(p); // parse til ')'
    case TOK_TYPE_RPAREN:
        parse_error(p, "expression", "')'");
    case TOK_TYPE_DOT:
        parse_error(p, "expression", "'.'");
    case TOK_TYPE_STR:
    case TOK_TYPE_INT:
    case TOK_TYPE_CONST:
    case TOK_TYPE_IDENT:
        return t.value;
    case TOK_TYPE_EOF:
        break;
    }
    return Qundef;
}

static Parser *parser_new(FILE *in)
{
    Parser *p = xmalloc(sizeof(Parser));
    p->in = in;
    p->prev_token = TOK_EOF; // we use this since we never postpone EOF things
    return p;
}

int64_t length(Value list)
{
    int64_t l = 0;
    for (; list != Qnil; list = cdr(list))
        l++;
    return l;
}

static void expect_arity_range(const char *func, int64_t min, int64_t max, Value args)
{
    int64_t actual = length(args);
    if (min <= actual && (max == -1 || actual <= max))
        return;
    runtime_error("%s: wrong number of arguments: expected %"PRId64"..%"PRId64" but got %"PRId64,
                  func, min, max, actual);
}

static void expect_arity(int64_t expected, Value args)
{
    int64_t actual = length(args);
    if (expected < 0 || expected == actual)
        return;
    runtime_error("wrong number of arguments: expected %"PRId64" but got %"PRId64,
                  expected, actual);
}

static Value apply_cfunc(Value *env, Value func, Value args)
{
    Value a[CFUNCARG_MAX];
    CFunc *cf = CFUNC(func);
    int64_t n = cf->func.arity;
    Value arg = args;
    for (int i = 0; i < n; i++) {
        a[i] = car(arg);
        arg = cdr(arg);
    }
    cfunc_t f = cf->cfunc;

#if defined(__clang__) && __clang_major__ >= 15
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wdeprecated-non-prototype"
#endif
    switch (n) {
    case -2:
        return (*f)(env, cdr(args)); // special form
    case -1:
        return (*f)(args); // non-special
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
        error("arity too large: %"PRId64, n);
    }
#if defined(__clang__) && __clang_major__ >= 15
#pragma clang diagnostic pop
#endif
}

static Value ieval(Value *env, Value v); // internal
static Value eval_body(Value *env, Value body);

inline static Value alist_prepend(Value list, Value key, Value val)
{
    return cons(cons(key, val), list);
}

static Value append(Value l1, Value l2)
{
    if (l2 == Qnil)
        return l1;
    if (l1 == Qnil)
        return l2;

    Value ret = Qnil, prev = Qnil;
    for (Value h = l1; h != Qnil; h = cdr(h)) {
        Value curr = cons(car(h), Qnil);
        if (ret == Qnil)
            ret = curr;
        if (prev != Qnil)
            PAIR(prev)->cdr = curr;
        prev = curr;
    }
    PAIR(prev)->cdr = l2;
    return ret;
}

static Value apply_closure(Value *env, Value func, Value args)
{
    Closure *cl = CLOSURE(func);
    int64_t arity = cl->func.arity;
    Value clenv = append(cl->env, *env), params = cl->params;
    if (arity == -1)
        clenv = alist_prepend(clenv, params, args);
    else {
        for (; args != Qnil; args = cdr(args), params = cdr(params))
            clenv = alist_prepend(clenv, car(params), car(args));
    }
    return eval_body(&clenv, cl->body);
}

ATTR_NORETURN ATTR(noinline)
static void jump(Continuation *cont)
{
    memcpy((void *) cont->sp, cont->shelter, cont->shelter_len);
    longjmp(cont->state, 1);
}

#define GET_SP(p) volatile void *p = &p

ATTR_NORETURN
static void apply_continuation(Value f, Value args)
{
    GET_SP(sp);
    Continuation *cont = CONTINUATION(f);
    cont->retval = car(args);
    int64_t d = sp - cont->sp;
    if (d < 1)
        d = 1;
    volatile uint8_t pad[d];
    pad[0] = pad[d-1] = 0; // avoid unused
    jump(cont);
}

static void expect_applicative(Value v)
{
    Type t = value_type_of(v);
    switch (t) {
    case TYPE_SPECIAL:
    case TYPE_CFUNC:
    case TYPE_CLOSURE:
    case TYPE_CONTINUATION:
        return;
    default:
        runtime_error("type error in (eval): expected applicative but got %s",
                      value_type_to_string(t));
    }
}

static Value map_eval(Value *env, Value l)
{
    Value mapped = Qnil, last = Qnil;
    for (; l != Qnil; l = cdr(l)) {
        last = append_at(last, ieval(env, car(l)));
        if (mapped == Qnil)
            mapped = last;
    }
    return mapped;
}

static Value apply(Value *env, Value func, Value args)
{
    expect_applicative(func);
    ValueTag tag = VALUE_TAG(func);
    Value eargs = (tag == TAG_SPECIAL) ?
        cons((Value) env, args) : map_eval(env, args);
    expect_arity(FUNCTION(func)->arity, eargs);
    switch (tag) {
    case TAG_SPECIAL:
    case TAG_CFUNC:
        return apply_cfunc(env, func, eargs);
    case TAG_CLOSURE:
        return apply_closure(env, func, eargs);
    case TAG_CONTINUATION:
        apply_continuation(func, eargs); // no return!
    default:
        UNREACHABLE();
    }
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

static void expect_cfunc_arity(int64_t actual)
{
    if (actual <= CFUNCARG_MAX)
        return;
    error("arity too large: expected ..%"PRId64" but got %"PRId64,
          CFUNCARG_MAX, actual);
}

static void env_put(Value *env, const char *name, Value val)
{
    *env = alist_prepend(*env, value_of_symbol(name), val);
}

static void define_special(Value *env, const char *name, cfunc_t cfunc, int64_t arity)
{
    expect_cfunc_arity(arity + 1);
    env_put(env, name, value_of_special(cfunc, arity));
}

static void define_function(Value *env, const char *name, cfunc_t cfunc, int64_t arity)
{
    expect_cfunc_arity(arity);
    env_put(env, name, value_of_cfunc(cfunc, arity));
}

static Value lookup(Value env, Value name)
{
    Value found = alist_find(env, name);
    if (found == Qnil)
        runtime_error("unbound variable: %s", value_to_string(name));
    return cdr(found);
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
    switch (value_type_of(v)) {
    case TYPE_BOOL:
        fprintf(f, "%s", v == Qtrue ? "#t" : "#f");
        break;
    case TYPE_INT:
        fprintf(f, "%"PRId64, value_to_int(v));
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
        fprintf(f, "%s", value_to_string(v));
        break;
    case TYPE_CFUNC:
        fprintf(f, "<c-function>");
        break;
    case TYPE_SPECIAL:
        fprintf(f, "<special>");
        break;
    case TYPE_CLOSURE:
        fprintf(f, "<closure>");
        break;
    case TYPE_CONTINUATION:
        fprintf(f, "<continuation>");
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

static Value iparse(FILE *in)
{
    if (setjmp(jmp_parse_error) != 0)
        return Qundef;
    Parser *p = parser_new(in);
    Value v = Qnil, last = Qnil;
    for (;;) {
        Value expr = parse_expr(p);
        if (expr == Qundef)
            break;
        last = append_at(last, expr);
        if (v == Qnil)
            v = last;
    }
    free(p);
    return v;
}

Value parse(const char *path)
{
    FILE *in = fopen(path, "r");
    if (in == NULL)
        error("load: can't open file: %s", path);
    Value retval = iparse(in);
    fclose(in);
    return retval;
}

Value parse_expr_string(const char *in)
{
    if (setjmp(jmp_parse_error) != 0)
        return Qundef;
    FILE *f = fmemopen((char *) in, strlen(in), "r");
    Parser *p = parser_new(f);
    Value v = parse_expr(p);
    free(p);
    fclose(f);
    return v;
}

Value parse_string(const char *in)
{
    FILE *f = fmemopen((char *) in, strlen(in), "r");
    Value v = iparse(f);
    fclose(f);
    return v;
}

//
// Evaluation
//

static Value eval_body(Value *env, Value body)
{
    Value last = Qnil;
    for (Value b = body; b != Qnil; b = cdr(b))
        last = ieval(env, car(b));
    return last;
}

static Value ieval(Value *env, Value v)
{
    if (value_is_symbol(v))
        return lookup(*env, v);
    if (v == Qnil || value_is_atom(v))
        return v;
    // else: function application
    Value func = ieval(env, car(v));
    return apply(env, func, cdr(v));
}

static Value eval_top(Value v)
{
    INIT_STACK();
    if (setjmp(jmp_runtime_error) != 0)
        return Qundef;
    return eval_body(&toplevel_environment, v);
}

Value eval(Value v)
{
    return eval_top(list(v));
}

static Value iload(FILE *in)
{
    Value l = iparse(in);
    if (l == Qundef)
        return Qundef;
    return eval_top(l);
}

static Value iload_inner(FILE *in)
{
    Value l = iparse(in);
    if (l == Qundef)
        return Qundef;
    return eval_body(&toplevel_environment, l);
}

Value eval_string(const char *in)
{
    FILE *f = fmemopen((char *) in, strlen(in), "r");
    Value v = iload(f);
    fclose(f);
    return v;
}

static FILE *open_loadable(const char *path, const char **basedir)
{
    char joined[PATH_MAX], rpath[PATH_MAX];
    snprintf(joined, sizeof(joined), "%s/%s", load_basedir, path);
    realpath(joined, rpath);

    FILE *in = fopen(rpath, "r");
    if (in == NULL)
        error("load: can't open file: %s", path);
    *basedir = dirname(rpath);
    return in;
}

// Current spec: path is always relative
Value load(const char *path)
{
    const char *basedir_saved = load_basedir;
    FILE *in = open_loadable(path, &load_basedir);
    Value retval = iload(in);
    fclose(in);
    load_basedir = basedir_saved;
    return retval;
}

static Value load_inner(const char *path)
{
    const char *basedir_saved = load_basedir;
    FILE *in = open_loadable(path, &load_basedir);
    Value retval = iload_inner(in);
    fclose(in);
    load_basedir = basedir_saved;
    return retval;
}

//
// Special Forms
//

static Value builtin_if(Value *env, Value args)
{
    expect_arity_range("if", 2, 3, args);

    Value cond = car(args), then = cadr(args);
    if (ieval(env, cond) != Qfalse)
        return ieval(env, then);
    Value els = cddr(args);
    if (els == Qnil)
        return Qnil;
    return ieval(env, car(els));
}

static Value define_variable(Value *env, Value ident, Value expr)
{
    expect_type("define", TYPE_SYMBOL, ident);

    Value val = ieval(env, expr), found;
    if (env == &toplevel_environment &&
        (found = alist_find(*env, ident)) != Qnil) {
        PAIR(found)->cdr = val; // set!
    } else
        *env = alist_prepend(*env, ident, val); // prepend new
    return Qnil;
}

static Value lambda(Value *env, Value params, Value body)
{
    expect_type_or("lambda", TYPE_PAIR, TYPE_SYMBOL, params);
    expect_type("lambda", TYPE_PAIR, body);
    if (body == Qnil)
        runtime_error("lambda: one or more expressions needed in body");
    return value_of_closure(*env, params, body);
}

static Value define_func_internal(Value *env, Value heads, Value body)
{
    Value ident = car(heads), params = cdr(heads);
    Value val = lambda(env, params, body);
    return define_variable(env, ident, val);
}

static Value builtin_define(Value *env, Value args)
{
    if (args == Qnil)
        runtime_error("define: wrong number of arguments: expected 1+");
    Value head = car(args);
    Type t = value_type_of(head);
    switch (t) {
    case TYPE_SYMBOL:
        expect_arity(2, args);
        return define_variable(env, head, cadr(args));
    case TYPE_PAIR:
        return define_func_internal(env, head, cdr(args));
    default:
        runtime_error("define: expected first argument symbol or pair but got %s",
                      value_type_to_string(t));
    }
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
        letenv = alist_prepend(letenv, ident, ieval(env, expr));
    }
    if (body == Qnil)
        runtime_error("let: one or more expressions needed in body");
    return eval_body(&letenv, body);
}

static Value builtin_letrec(Value *env, Value args)
{
    Value bindings = car(args);
    Value body = cdr(args);
    expect_type_twin("letrec", TYPE_PAIR, bindings, body);

    Value letenv = *env;
    for (Value b = bindings; b != Qnil; b = cdr(b)) {
        Value p = car(b);
        expect_type("letrec", TYPE_PAIR, p);
        Value ident = car(p);
        expect_type("letrec", TYPE_SYMBOL, ident);
        Value val = ieval(&letenv, cadr(p));
        letenv = alist_prepend(letenv, ident, val);
    }
    if (body == Qnil)
        runtime_error("letrec: one or more expressions needed in body");
    return eval_body(&letenv, body);
}

static Value builtin_begin(Value *env, Value body)
{
    return eval_body(env, body);
}

static Value builtin_cond(Value *env, Value clauses)
{
    expect_arity_range("cond", 1, -1, clauses);

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

static Value builtin_lambda(Value *env, Value args)
{
    return lambda(env, car(args), cdr(args));
}

static Value value_of_continuation(void)
{
    Continuation *c = obj_new(sizeof(Continuation), TAG_CONTINUATION);
    c->func.arity = 1; // by spec
    return (Value) c;
}

static bool continuation_set(Value c)
{
    GET_SP(sp); // must be the first!
    Continuation *cont = CONTINUATION(c);
    cont->sp = sp;
    cont->shelter_len = stack_base - sp;
    cont->shelter = xmalloc(cont->shelter_len);
    memcpy(cont->shelter, (void *) sp, cont->shelter_len);
    return setjmp(cont->state);
}

static Value builtin_callcc(Value *env, Value f)
{
    Value cl = ieval(env, f);
    expect_type("call/cc", TYPE_CLOSURE, cl);
    Value c = value_of_continuation();
    if (continuation_set(c) != 0)
        return CONTINUATION(c)->retval;
    return apply_closure(env, cl, cons(c, Qnil));
}

//
// Built-in Functions: Arithmetic
//

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
    expect_arity_range("-", 1, -1, args);

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
    expect_arity_range("/", 1, -1, args);

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
    expect_arity_range("=", 2, -1, args);

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
    expect_arity_range("<", 2, -1, args);

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
    expect_arity_range(">", 2, -1, args);

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
    expect_arity_range("<=", 2, -1, args);

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
    expect_arity_range(">=", 2, -1, args);

    int64_t x = value_get_int(">=", car(args));
    while ((args = cdr(args)) != Qnil) {
        int64_t y = value_get_int(">=", car(args));
        if (x < y)
            return Qfalse;
        x = y;
    }
    return Qtrue;
}

static Value builtin_modulo(Value x, Value y)
{
    int64_t b = value_get_int("modulo", y);
    if (b == 0)
        runtime_error("modulo: divided by zero");
    int64_t a = value_get_int("modulo", x);
    int64_t c = a % b;
    if ((a < 0 && b > 0) || (a > 0 && b < 0))
        c += b;
    return value_of_int(c);
}

static Value builtin_not(Value x)
{
    return OF_BOOL(x == Qfalse);
}

//
// Built-in Functions: Lists and others
//

static Value builtin_list(Value args)
{
    return args;
}

static Value builtin_null(Value list)
{
    return OF_BOOL(list == Qnil);
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

static Value builtin_print(Value obj)
{
    display(obj);
    puts("");
    return obj;
}

static Value builtin_car(Value pair)
{
    expect_type("car", TYPE_PAIR, pair);
    return car(pair);
}

static Value builtin_cdr(Value pair)
{
    expect_type("cdr", TYPE_PAIR, pair);
    return cdr(pair);
}

static Value builtin_eq(Value x, Value y)
{
    return OF_BOOL(x == y);
}

static bool equal(Value x, Value y)
{
    if (x == y)
        return true;
    Type tx = value_type_of(x), ty = value_type_of(y);
    if (tx != ty)
        return false;
    switch (tx) {
    case TYPE_PAIR:
        if (x == Qnil || y == Qnil)
            return false;
        return equal(car(x), car(y)) &&
               equal(cdr(x), cdr(y));
    case TYPE_STR:
        return (strcmp(STRING(x)->body, STRING(y)->body) == 0);
    default:
        return false;
    }
}

static Value builtin_equal(Value x, Value y)
{
    return OF_BOOL(equal(x, y));
}

static Value builtin_load(Value path)
{
    return load_inner(value_to_string(path));
}

//
// Built-in Functions: Extensions
//

static Value builtin_cputime(void) // in micro sec
{
    static const int64_t MICRO = 1000*1000;
    struct timespec t;
    clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &t);
    int64_t n = t.tv_sec * MICRO + lround(t.tv_nsec / 1000.0);
    return value_of_int(n);
}

ATTR_CTOR
static void initialize(void)
{
    static char basedir[PATH_MAX];
    load_basedir = getcwd(basedir, sizeof(basedir));
    SYM_ELSE = value_of_symbol("else");

    Value *e = &toplevel_environment;
    define_special(e, "if", builtin_if, -1);
    define_special(e, "define", builtin_define, -1);
    define_special(e, "set!", builtin_set, 2);
    define_special(e, "let", builtin_let, -1);
    define_special(e, "let*", builtin_let, -1); // alias
    define_special(e, "letrec", builtin_letrec, -1);
    define_special(e, "begin", builtin_begin, -1);
    define_special(e, "cond", builtin_cond, -1);
    define_special(e, "lambda", builtin_lambda, -1);
    define_special(e, "call/cc", builtin_callcc, 1);

    define_function(e, "+", builtin_add, -1);
    define_function(e, "-", builtin_sub, -1);
    define_function(e, "*", builtin_mul, -1);
    define_function(e, "/", builtin_div, -1);
    define_function(e, "=", builtin_numeq, -1);
    define_function(e, "<", builtin_lt, -1);
    define_function(e, ">", builtin_gt, -1);
    define_function(e, "<=", builtin_le, -1);
    define_function(e, ">=", builtin_ge, -1);
    define_function(e, "modulo", builtin_modulo, 2);
    define_function(e, "not", builtin_not, 1);

    define_function(e, "car", builtin_car, 1);
    define_function(e, "cdr", builtin_cdr, 1);
    define_function(e, "cons", cons, 2);
    define_function(e, "list", builtin_list, -1);
    define_function(e, "null?", builtin_null, 1);
    define_function(e, "reverse", reverse, 1);
    define_function(e, "display", builtin_display, 1);
    define_function(e, "newline", builtin_newline, 0);
    define_function(e, "print", builtin_print, 1);
    define_function(e, "eq?", builtin_eq, 2);
    define_function(e, "equal?", builtin_equal, 2);
    define_function(e, "load", builtin_load, 1);

    define_function(e, "_cputime", builtin_cputime, 0);
}

// for testing
void reset_environment(void)
{
    toplevel_environment = Qnil;
    initialize();
}
