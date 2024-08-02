#ifndef LISP_H
#define LISP_H

#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>

#include "utils.h"

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
    TYPE_CONT,
} Type;

typedef struct Pair Pair;
typedef uintptr_t Value;
typedef uintptr_t Symbol;
#define ANYARGS /*empty*/
typedef Value (*CFunc)(ANYARGS);

extern const Value Qnil, Qundef, Qfalse, Qtrue;

bool value_is_int(Value v);
bool value_is_symbol(Value v);
bool value_is_string(Value v);
bool value_is_cfunc(Value v);
bool value_is_closure(Value v);
bool value_is_atom(Value v);
bool value_is_pair(Value v);
bool value_is_nil(Value v);
Type value_type_of(Value v);
const char *value_type_to_string(Type t);

int64_t value_to_int(Value v);
Symbol value_to_symbol(Value v);
const char *value_to_string(Value v);

Value value_of_int(int64_t i);
Value value_of_symbol(const char *s);
Value value_of_string(const char *s);

Value cons(Value car, Value cdr);
Value list(Value v, ...); // terminate with Qundef
int64_t length(Value list);
Value reverse(Value v);

Value car(Value v);
Value cdr(Value v);
Value caar(Value v); // 2
Value cadr(Value v);
Value cdar(Value v);
Value cddr(Value v);
Value caaar(Value v); // 3
Value caadr(Value v);
Value cadar(Value v);
Value caddr(Value v);
Value cdaar(Value v);
Value cdadr(Value v);
Value cddar(Value v);
Value cdddr(Value v);
Value caaaar(Value v); // 4
Value caaadr(Value v);
Value caadar(Value v);
Value caaddr(Value v);
Value cadaar(Value v);
Value cadadr(Value v);
Value caddar(Value v);
Value cadddr(Value v);
Value cdaaar(Value v);
Value cdaadr(Value v);
Value cdadar(Value v);
Value cdaddr(Value v);
Value cddaar(Value v);
Value cddadr(Value v);
Value cdddar(Value v);
Value cddddr(Value v);

void display(Value v);
Value parse(FILE *in);
Value load(FILE *in);
Value eval(Value v);
Value eval_string(const char *s);

ATTR_MALLOC char *stringify(Value v);
Value parse_string(const char *in);
Value parse_expr_string(const char *in);

const char *error_message(void);

void reset_environment(void); // for testing

#endif
