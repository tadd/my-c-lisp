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
// Types
//

static const char *TYPE_NAMES[] = {
    [TYPE_BOOL] = "boolean",
    [TYPE_INT] = "integer",
    [TYPE_SYMBOL] = "symbol",
    [TYPE_UNDEF] = "undef",
    [TYPE_PAIR] = "pair",
    [TYPE_STR] = "string",
    [TYPE_PROC] = "procedure",
};

typedef enum { // has the same values as Type
    TAG_PAIR    = TYPE_PAIR,
    TAG_STR     = TYPE_STR,
    TAG_CFUNC   = TYPE_PROC + 1,
    TAG_SYNTAX, // almost a C Function
    TAG_CLOSURE,
    TAG_CONTINUATION,
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
} Procedure;

typedef struct {
    Procedure proc;
    cfunc_t cfunc;
} CFunc;

typedef struct {
    Procedure proc;
    Value env;
    Value params;
    Value body;
} Closure;

typedef struct {
    Procedure proc;
    volatile void *sp;
    void *shelter;
    size_t shelter_len;
    jmp_buf state;
    Value retval;
} Continuation;

#define VALUE_TAG(v) (*(ValueTag*)(v))
#define PAIR(v) ((Pair *) v)
#define STRING(v) ((String *) v)
#define PROCEDURE(v) ((Procedure *) v)
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
//   0b0--0010 #f
//   0b0--0100 #t
//   0b0--0110 <undef>
typedef const uintptr_t Flag;
static Flag FLAG_NBIT_SYM = 4;
static Flag FLAG_NBIT_INT = 1;
static Flag FLAG_MASK     =  0b111; // for 64 bit machine
static Flag FLAG_MASK_SYM = 0b1111;
static Flag FLAG_MASK_INT =    0b1;
static Flag FLAG_SYM      = 0b1110;
static Flag FLAG_INT      =    0b1;
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
static Value SYM_QUOTE;
static const volatile void *stack_base = NULL;
#define INIT_STACK() void *basis; stack_base = &basis
static const char *load_basedir = NULL;

//
// value_is_*: Type Checks
//

inline bool value_is_int(Value v)
{
    return v & FLAG_MASK_INT;
}

inline bool value_is_symbol(Value v)
{
    return (v & FLAG_MASK_SYM) == FLAG_SYM;
}

static inline bool is_immediate(Value v)
{
    return v & FLAG_MASK;
}

static inline bool value_tag_is(Value v, ValueTag expected)
{
    return !is_immediate(v) && VALUE_TAG(v) == expected;
}

inline bool value_is_string(Value v)
{
    return value_tag_is(v, TAG_STR);
}

inline bool value_is_pair(Value v)
{
    return value_tag_is(v, TAG_PAIR);
}

static inline bool value_is_atom(Value v)
{
    return !value_is_pair(v);
}

inline bool value_is_nil(Value v)
{
    return v == Qnil;
}

static Type immediate_type_of(Value v)
{
    if (value_is_int(v))
        return TYPE_INT;
    if (value_is_symbol(v))
        return TYPE_SYMBOL;
    if (v == Qtrue || v == Qfalse)
        return TYPE_BOOL;
    if (v == Qundef)
        return TYPE_UNDEF;
    UNREACHABLE();
}

Type value_type_of(Value v)
{
    if (is_immediate(v))
        return immediate_type_of(v);
    ValueTag t = VALUE_TAG(v);
    switch (t) {
    case TAG_STR:
    case TAG_PAIR:
        return (Type) t;
    case TAG_CFUNC:
    case TAG_SYNTAX:
    case TAG_CLOSURE:
    case TAG_CONTINUATION:
        return TYPE_PROC;
    }
    UNREACHABLE();
}

inline const char *value_type_to_string(Type t)
{
    return TYPE_NAMES[t];
}

// value_to_*: Convert internal data to external plain C

inline int64_t value_to_int(Value x)
{
#if __x86_64__
    return (int64_t) x >> FLAG_NBIT_INT;
#else
    int64_t i = x;
    return (i - 1) / (1 << FLAG_NBIT_INT);
#endif
}

inline Symbol value_to_symbol(Value v)
{
    return (Symbol) v >> FLAG_NBIT_SYM;
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
    if (name == NULL) // fatal; every known symbols should have a name
        error("symbol %lu not found", sym);
    return name;
}

inline const char *value_to_string(Value v)
{
    if (value_is_symbol(v))
        return unintern(value_to_symbol(v));
    return STRING(v)->body;
}

// value_of_*: Convert external plain C data to internal

inline Value value_of_int(int64_t i)
{
    Value v = i;
    return v << FLAG_NBIT_INT | FLAG_INT;
}

static inline Value list1(Value x)
{
    return cons(x, Qnil);
}

static Symbol intern(const char *name)
{
    Value last = Qnil;
    int64_t i = 0;
    // find
    for (Value p = symbol_names; p != Qnil; last = p, p = cdr(p)) {
        Value v = car(p);
        if (strcmp(STRING(v)->body, name) == 0)
            return i;
        i++;
    }
    // or put at `i`
    Value s = value_of_string(name);
    Value next = list1(s);
    if (last == Qnil)
        symbol_names = next;
    else
        PAIR(last)->cdr = next;
    return i;
}

inline Value value_of_symbol(const char *s)
{
    Symbol sym = intern(s);
    return (Value) (sym << FLAG_NBIT_SYM | FLAG_SYM);
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

static void expect_cfunc_arity(int64_t actual)
{
    if (actual <= CFUNCARG_MAX)
        return;
    error("arity too large: expected ..%"PRId64" but got %"PRId64,
          CFUNCARG_MAX, actual);
}

static Value value_of_cfunc(cfunc_t cfunc, int64_t arity)
{
    expect_cfunc_arity(arity);
    CFunc *f = obj_new(sizeof(CFunc), TAG_CFUNC);
    f->proc.arity = arity;
    f->cfunc = cfunc;
    return (Value) f;
}

static Value value_of_syntax(cfunc_t cfunc, int64_t arity)
{
    Value sp = value_of_cfunc(cfunc, arity);
    VALUE_TAG(sp) = TAG_SYNTAX;
    return sp;
}

static Value value_of_closure(Value env, Value params, Value body)
{
    Closure *f = obj_new(sizeof(Closure), TAG_CLOSURE);
    f->proc.arity = (value_type_of(params) == TYPE_PAIR) ? length(params) : -1;
    f->env = env;
    f->params = params;
    f->body = body;
    return (Value) f;
}

// and `cons` is well-known name than "value_of_pair"

//
// Errors
//

#define error(fmt, ...) \
    error("%s:%d of %s: " fmt, __FILE__, __LINE__, __func__ __VA_OPT__(,) __VA_ARGS__)

static jmp_buf jmp_runtime_error, jmp_parse_error;
static char errmsg[BUFSIZ];

ATTR(noreturn)
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

//
// Parse
//

typedef enum {
    TOK_TYPE_LPAREN,
    TOK_TYPE_RPAREN,
    TOK_TYPE_QUOTE,
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
    TOK_QUOTE = TOK(QUOTE),
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

static Token get_token_dots(Parser *p)
{
    int c = fgetc(p->in);
    if (c != '.') {
        ungetc(c, p->in);
        return TOK_DOT;
    }
    c = fgetc(p->in);
    if (c != '.') {
        ungetc(c, p->in);
        return TOK_IDENT("..");
    }
    return TOK_IDENT("...");
}

static Token get_token_string(Parser *p)
{
    char buf[BUFSIZ], *pbuf = buf, *end = pbuf + sizeof(buf) - 2;
    for (int c; (c = fgetc(p->in)) != '"'; *pbuf++ = c) {
        if (c == '\\') {
            c = fgetc(p->in);
            if (c != '\\' && c != '"')
                parse_error(p, "'\\' or '\"' in string literal", "'%c'", c);
        }
        if (pbuf == end)
            parse_error(p, "string literal", "too long: \"%s...\"", pbuf);
    }
    *pbuf = '\0';
    return TOK_STR(buf);
}

static Token get_token_constant(Parser *p)
{
    int c = fgetc(p->in);
    switch (c) {
    case 't':
        return TOK_CONST(Qtrue);
    case 'f':
        return TOK_CONST(Qfalse);
    default:
        parse_error(p, "constants", "#%c", c);
    }
}

static Token get_token_int(Parser *p, int c, int sign)
{
    ungetc(c, p->in);
    int64_t i;
    int n = fscanf(p->in, "%"SCNd64, &i);
    if (n != 1)
        parse_error(p, "integer", "invalid string");
    return TOK_INT(sign * i);
}

static Token get_token_after_sign(Parser *p, int csign)
{
    int c = fgetc(p->in);
    int dig = isdigit(c);
    if (dig) {
        int sign = csign == '-' ? -1 : 1;
        return get_token_int(p, c, sign);
    }
    ungetc(c, p->in);
    return TOK_IDENT(((char []) { csign, '\0' }));
}

static inline bool is_special_initial(int c)
{
    switch (c) {
    case '!': case '$': case '%': case '&': case '*': case '/': case ':':
    case '<': case '=': case '>': case '?': case '^': case '_': case '~':
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
    return c == '+' || c == '-' || c == '.' || c == '@';
}

static inline bool is_subsequent(int c)
{
    return is_initial(c) || isdigit(c) || is_special_subsequent(c);
}

static Token get_token_ident(Parser *p, int init)
{
    char buf[BUFSIZ], *s = buf, *end = s + sizeof(buf);
    int c;
    for (*s++ = init; is_subsequent(c = fgetc(p->in)); *s++ = c) {
        if (s == end)
            parse_error(p, "identifier", "too long");
    }
    ungetc(c, p->in);
    *s = '\0';
    return TOK_IDENT(buf);
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
    case '\'':
        return TOK_QUOTE;
    case '.':
        return get_token_dots(p);
    case '"':
        return get_token_string(p);
    case '#':
        return get_token_constant(p);
    case '+':
    case '-':
        return get_token_after_sign(p, c);
    case EOF:
        return TOK_EOF;
    default:
        break;
    }
    if (isdigit(c))
        return get_token_int(p, c, 1);
    if (is_initial(c))
        return get_token_ident(p, c);
    parse_error(p, "valid char", "'%c'", c);
}

static void unget_token(Parser *p, Token t)
{
    p->prev_token = t;
}

#define CXR1(f, x) f(a, x); f(d, x);
#define CXR2(f, x) CXR1(f, d ## x) //CXR1(f, a ## x) 
#define CXRS(f) CXR2(f,)

#define DEF_CXR(x, y) \
    static Value c##x##y##r(Value v) { return c##x##r(c##y##r(v)); }
CXRS(DEF_CXR)

static const char *token_stringify(Token t)
{
    static char buf[BUFSIZ];

    switch (t.type) {
    case TOK_TYPE_LPAREN:
        return "(";
    case TOK_TYPE_RPAREN:
        return ")";
    case TOK_TYPE_QUOTE:
        return "'";
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

static Value parse_expr(Parser *p);

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

static Value append_at(Value last, Value elem)
{
    Value p = list1(elem);
    if (last != Qnil)
        PAIR(last)->cdr = p;
    return p;
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

static inline Value list2(Value x, Value y)
{
    return cons(x, cons(y, Qnil));
}

static Value parse_quoted(Parser *p, Value sym)
{
    Value e = parse_expr(p);
    if (e == Qundef)
        parse_error(p, "expression", "'EOF'");
    return list2(sym, e);
}

static Value parse_expr(Parser *p)
{
    Token t = get_token(p);
    switch (t.type) {
    case TOK_TYPE_LPAREN:
        return parse_list(p); // parse til ')'
    case TOK_TYPE_RPAREN:
        parse_error(p, "expression", "')'");
    case TOK_TYPE_QUOTE:
        return parse_quoted(p, SYM_QUOTE);
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

static Value apply_cfunc(Value *env, Value proc, Value args)
{
    Value a[CFUNCARG_MAX];
    CFunc *cf = CFUNC(proc);
    int64_t n = cf->proc.arity;
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
    case -1:
        return (*f)(env, args);
    case 0:
        return (*f)(env);
    case 1:
        return (*f)(env, a[0]);
    case 2:
        return (*f)(env, a[0], a[1]);
    case 3:
        return (*f)(env, a[0], a[1], a[2]);
    case 4:
        return (*f)(env, a[0], a[1], a[2], a[3]);
    case 5:
        return (*f)(env, a[0], a[1], a[2], a[3], a[4]);
    case 6:
        return (*f)(env, a[0], a[1], a[2], a[3], a[4], a[5]);
    case 7:
        return (*f)(env, a[0], a[1], a[2], a[3], a[4], a[5], a[6]);
    default:
        error("arity too large: %"PRId64, n);
    }
#if defined(__clang__) && __clang_major__ >= 15
#pragma clang diagnostic pop
#endif
}

static inline void env_put(Value *env, Value name, Value val)
{
    *env = cons(cons(name, val), *env);
}

static Value append2(Value l1, Value l2)
{
    if (l2 == Qnil)
        return l1;
    if (l1 == Qnil)
        return l2;

    Value ret = Qnil, prev = Qnil;
    for (Value p = l1; p != Qnil; p = cdr(p)) {
        Value curr = list1(car(p));
        if (ret == Qnil)
            ret = curr;
        if (prev != Qnil)
            PAIR(prev)->cdr = curr;
        prev = curr;
    }
    PAIR(prev)->cdr = l2;
    return ret;
}

static Value eval_body(Value *env, Value body);

static Value apply_closure(Value *env, Value proc, Value args)
{
    Closure *cl = CLOSURE(proc);
    int64_t arity = cl->proc.arity;
    Value clenv = append2(cl->env, *env), params = cl->params;
    if (arity == -1)
        env_put(&clenv, params, args);
    else {
        for (Value p = args; p != Qnil; p = cdr(p), params = cdr(params))
            env_put(&clenv, car(params), car(p));
    }
    return eval_body(&clenv, cl->body);
}

ATTR(noreturn) ATTR(noinline)
static void jump(Continuation *cont)
{
    memcpy((void *) cont->sp, cont->shelter, cont->shelter_len);
    longjmp(cont->state, 1);
}

#define GET_SP(p) volatile void *p = &p

ATTR(noreturn)
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

static Value apply(Value *env, Value proc, Value args)
{
    expect_type("apply", TYPE_PROC, proc);
    expect_arity(PROCEDURE(proc)->arity, args);
    switch (VALUE_TAG(proc)) {
    case TAG_SYNTAX:
    case TAG_CFUNC:
        return apply_cfunc(env, proc, args);
    case TAG_CLOSURE:
        return apply_closure(env, proc, args);
    case TAG_CONTINUATION:
        apply_continuation(proc, args); // no return!
    default:
        UNREACHABLE();
    }
}

static Value assq(Value key, Value l);

// Note: Do not mistake this for "(define-syntax ...)" which related to macros
static void define_syntax(Value *env, const char *name, cfunc_t cfunc, int64_t arity)
{
    env_put(env, value_of_symbol(name), value_of_syntax(cfunc, arity));
}

static void define_procedure(Value *env, const char *name, cfunc_t cfunc, int64_t arity)
{
    env_put(env, value_of_symbol(name), value_of_cfunc(cfunc, arity));
}

static Value lookup(Value env, Value name)
{
    Value found = assq(name, env);
    if (found == Qfalse)
        runtime_error("unbound variable: %s", value_to_string(name));
    return cdr(found);
}

static Value iparse(FILE *in)
{
    if (setjmp(jmp_parse_error) != 0)
        return Qundef;
    Parser *p = parser_new(in);
    Value v = Qnil, last = Qnil;
    for (Value expr; (expr = parse_expr(p)) != Qundef; ) {
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
        error("parse: can't open file: %s", path);
    Value retval = iparse(in);
    fclose(in);
    return retval;
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
static Value ieval(Value *env, Value v); // internal

static Value eval_body(Value *env, Value body)
{
    Value last = Qnil;
    for (Value p = body; p != Qnil; p = cdr(p))
        last = ieval(env, car(p));
    return last;
}

static Value map_eval(Value *env, Value l)
{
    Value mapped = Qnil, last = Qnil;
    for (Value p = l; p != Qnil; p = cdr(p)) {
        last = append_at(last, ieval(env, car(p)));
        if (mapped == Qnil)
            mapped = last;
    }
    return mapped;
}

static Value eval_apply(Value *env, Value symproc, Value args)
{
    Value proc = ieval(env, symproc);
    if (!value_tag_is(proc, TAG_SYNTAX))
        args = map_eval(env, args);
    return apply(env, proc, args);
}

static Value ieval(Value *env, Value v)
{
    if (value_is_symbol(v))
        return lookup(*env, v);
    if (v == Qnil || value_is_atom(v))
        return v;
    return eval_apply(env, car(v), cdr(v));
}

static Value iload(FILE *in)
{
    Value l = iparse(in);
    if (l == Qundef)
        return Qundef;
    if (setjmp(jmp_runtime_error) != 0)
        return Qundef;
    INIT_STACK();
    return eval_body(&toplevel_environment, l);
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

static FILE *open_loadable(const char *path)
{
    static char rpath[PATH_MAX];
    char joined[PATH_MAX];
    snprintf(joined, sizeof(joined), "%s/%s", load_basedir, path);
    realpath(joined, rpath);

    FILE *in = fopen(rpath, "r");
    if (in == NULL)
        error("load: can't open file: %s", path);
    load_basedir = dirname(rpath);
    return in;
}

Value load(const char *path)
{
    FILE *in = open_loadable(path);
    Value retval = iload(in);
    fclose(in);
    return retval;
}

static Value load_inner(const char *path)
{
    const char *basedir_saved = load_basedir;
    FILE *in = open_loadable(path);
    Value retval = iload_inner(in);
    fclose(in);
    load_basedir = basedir_saved;
    return retval;
}

//
// Built-in Procedures / Syntax
//
#define UNUSED ATTR(unused)

// 4.1. Primitive expression types
// 4.1.2. Literal expressions
static Value syn_quote(UNUSED Value *env, Value datum)
{
    return datum;
}

// 4.1.4. Procedures
static Value lambda(Value *env, Value params, Value body)
{
    expect_type_or("lambda", TYPE_PAIR, TYPE_SYMBOL, params);
    expect_type("lambda", TYPE_PAIR, body);
    if (body == Qnil)
        runtime_error("lambda: one or more expressions needed in body");
    return value_of_closure(*env, params, body);
}

static Value syn_lambda(Value *env, Value args)
{
    return lambda(env, car(args), cdr(args));
}

// 4.1.5. Conditionals
static Value syn_if(Value *env, Value args)
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

// 4.1.6. Assignments
static Value iset(Value *env, Value ident, Value val)
{
    Value found = assq(ident, *env);
    if (found == Qfalse)
        runtime_error("set!: unbound variable: %s", value_to_string(ident));
    PAIR(found)->cdr = val;
    return Qnil;
}

static Value syn_set(Value *env, Value ident, Value expr)
{
    expect_type("set!", TYPE_SYMBOL, ident);
    return iset(env, ident, ieval(env, expr));
}

// 4.2. Derived expression types
// 4.2.1. Conditionals
static inline void expect_nonnull(const char *msg, Value l)
{
    expect_type(msg, TYPE_PAIR, l);
    if (l == Qnil)
        runtime_error("%s: expected non-null?", msg);
}

// 4.2. Derived expression types
// 4.2.1. Conditionals
static Value syn_and(Value *env, Value args)
{
    Value last = Qtrue;
    for (Value p = args; p != Qnil; p = cdr(p)) {
        if ((last = ieval(env, car(p))) == Qfalse)
            break;
    }
    return last;
}

static Value syn_or(UNUSED Value *env, Value args)
{
    for (Value p = args, curr; p != Qnil; p = cdr(p)) {
        if ((curr = ieval(env, car(p))) != Qfalse)
            return curr;
    }
    return Qfalse;
}

static Value define_variable(Value *env, Value ident, Value expr);

static Value let(Value *env, const char *func, Value bindings, Value body)
{
    expect_type(func, TYPE_PAIR, bindings);
    Value letenv = *env;
    for (Value p = bindings; p != Qnil; p = cdr(p)) {
        Value b = car(p);
        if (b == Qnil)
            continue;
        expect_type(func, TYPE_PAIR, b);
        Value ident = car(b), expr = cadr(b);
        expect_type(func, TYPE_SYMBOL, ident);
        env_put(&letenv, ident, ieval(env, expr));
    }
    return eval_body(&letenv, body);
}

static Value syn_let(Value *env, Value args)
{
    expect_arity_range("let", 2, -1, args);
    Value binds = car(args), body = cdr(args);
    return let(env, "let", binds, body);
}

// 5.2. Definitions
static Value define_variable(Value *env, Value ident, Value expr)
{
    expect_type("define", TYPE_SYMBOL, ident);

    Value val = ieval(env, expr), found;
    if (env == &toplevel_environment &&
        (found = assq(ident, *env)) != Qfalse) {
        PAIR(found)->cdr = val; // set!
    } else
        env_put(env, ident, val); // prepend new
    return Qnil;
}

static Value define_proc_internal(Value *env, Value heads, Value body)
{
    Value ident = car(heads), params = cdr(heads);
    Value val = lambda(env, params, body);
    return define_variable(env, ident, val);
}

static Value syn_define(Value *env, Value args)
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
        return define_proc_internal(env, head, cdr(args));
    default:
        runtime_error("define: expected first argument symbol or pair but got %s",
                      value_type_to_string(t));
    }
}

// 6.1. Equivalence predicates
static inline bool eq(Value x, Value y)
{
    return x == y;
}

static Value proc_eq(UNUSED Value *env, Value x, Value y)
{
    return OF_BOOL(eq(x, y));
}

// 6.3.2. Pairs and lists
static Value proc_pair_p(UNUSED Value *env, Value o)
{
    return OF_BOOL(o != Qnil && value_is_pair(o));
}

Value cons(Value car, Value cdr)
{
    Pair *p = obj_new(sizeof(Pair), TAG_PAIR);
    p->car = car;
    p->cdr = cdr;
    return (Value) p;
}

inline Value car(Value v)
{
    return PAIR(v)->car;
}

inline Value cdr(Value v)
{
    return PAIR(v)->cdr;
}

static Value proc_cons(UNUSED Value *env, Value car, Value cdr)
{
    return cons(car, cdr);
}

static Value proc_car(UNUSED Value *env, Value pair)
{
    expect_nonnull("car", pair);
    return car(pair);
}

static Value proc_cdr(UNUSED Value *env, Value pair)
{
    expect_nonnull("cdr", pair);
    return cdr(pair);
}

static Value proc_list_p(UNUSED Value *env, Value list)
{
    for (Value p = list; p != Qnil; p = cdr(p)) {
        if (!value_is_pair(p))
            return Qfalse;
    }
    return Qtrue;
}

// C API-level utility
Value list(Value arg, ...)
{
    Value l = Qnil, last = l;
    va_list ap;
    va_start(ap, arg);
    for (Value o = arg; o != Qundef; o = va_arg(ap, Value)) {
        last = append_at(last, o);
        if (l == Qnil)
            l = last;
    }
    va_end(ap);
    return l;
}

int64_t length(Value list)
{
    int64_t len = 0;
    for (Value p = list; p != Qnil; p = cdr(p))
        len++;
    return len;
}

static Value reverse(Value l)
{
    Value ret = Qnil;
    for (Value p = l; p != Qnil; p = cdr(p))
        ret = cons(car(p), ret);
    return ret;
}

static Value proc_reverse(UNUSED Value *env, Value list)
{
    expect_type("reverse", TYPE_PAIR, list);
    return reverse(list);
}

static Value assq(Value key, Value l)
{
    for (Value p = l; p != Qnil; p = cdr(p)) {
        Value entry = car(p);
        if (value_is_pair(entry) && car(entry) == key)
            return entry;
    }
    return Qfalse;
}

static Value apply_args(Value args)
{
    Value heads = Qnil, last = Qnil, p, next;
    for (p = args; (next = cdr(p)) != Qnil; p = next) {
        last = append_at(last, car(p));
        if (heads == Qnil)
            heads = last;
    }
    Value rest = car(p);
    expect_type("args on apply", TYPE_PAIR, rest);
    return append2(heads, rest);
}

static Value proc_apply(Value *env, Value args)
{
    expect_arity_range("apply", 2, -1, args);

    Value proc = car(args);
    expect_type("apply", TYPE_PROC, proc);
    Value appargs = apply_args(cdr(args));
    return apply(env, proc, appargs);
}

// 6.6.3. Output
static void fdisplay(FILE* f, Value v);

static void display_list(FILE *f, Value l)
{
    fprintf(f, "(");
    for (Value p = l, next; p != Qnil; p = next) {
        fdisplay(f, car(p));
        if ((next = cdr(p)) == Qnil)
            break;
        fprintf(f, " ");
        if (value_is_atom(next)) {
            fprintf(f, ". ");
            fdisplay(f, next);
            break;
        }
    }
    fprintf(f, ")");
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
    case TYPE_STR:
        fprintf(f, "%s", value_to_string(v));
        break;
    case TYPE_PAIR:
        display_list(f, v);
        break;
    case TYPE_PROC:
        fprintf(f, "<procedure>");
        break;
    case TYPE_UNDEF:
        fprintf(f, "<undef>");
        break;
    }
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

void display(Value v)
{
    fdisplay(stdout, v);
}

static Value proc_display(UNUSED Value *env, Value obj)
{
    display(obj);
    return obj;
}

// 6.6.4. System interface
static Value proc_load(UNUSED Value *env, Value path)
{
    // Current spec: path is always relative
    return load_inner(value_to_string(path));
}

ATTR(constructor)
static void initialize(void)
{
    static char basedir[PATH_MAX];
    load_basedir = getcwd(basedir, sizeof(basedir));
    SYM_QUOTE = value_of_symbol("quote");

    Value *e = &toplevel_environment;

    // 4. Expressions

    // 4.1. Primitive expression types
    // 4.1.2. Literal expressions
    define_syntax(e, "quote", syn_quote, 1);
    // 4.1.4. Procedures
    define_syntax(e, "lambda", syn_lambda, -1);
    // 4.1.5. Conditionals
    define_syntax(e, "if", syn_if, -1);
    // 4.1.6. Assignments
    define_syntax(e, "set!", syn_set, 2);
    // 4.2. Derived expression types
    // 4.2.1. Conditionals
    define_syntax(e, "and", syn_and, -1);
    define_syntax(e, "or", syn_or, -1);
    // 4.2.2. Binding constructs
    define_syntax(e, "let", syn_let, -1); // with named let in 4.2.4.

    // 5. Program structure

    // 5.2. Definitions
    define_syntax(e, "define", syn_define, -1);
    // 5.3. Syntax definitions
    //- define-syntax

    // 6. Standard procedures

    // 6.1. Equivalence predicates
    define_procedure(e, "eq?", proc_eq, 2);
    // 6.3.2. Pairs and lists
    define_procedure(e, "pair?", proc_pair_p, 1);
    define_procedure(e, "cons", proc_cons, 2);
    define_procedure(e, "car", proc_car, 1);
    define_procedure(e, "cdr", proc_cdr, 1);
    define_procedure(e, "list?", proc_list_p, 1);
    define_procedure(e, "reverse", proc_reverse, 1);
    // 6.4. Control features
    define_procedure(e, "apply", proc_apply, -1);
    // 6.6.3. Output
    define_procedure(e, "display", proc_display, 1);
    // 6.6.4. System interface
    define_procedure(e, "load", proc_load, 1);
}
