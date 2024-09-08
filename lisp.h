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
    TYPE_PROC,
} Type;

typedef struct Pair Pair;
typedef uintptr_t Value;
typedef uintptr_t Symbol;
typedef Value (*cfunc_t)(/*ANYARGS*/);

extern const Value Qnil, Qundef, Qfalse, Qtrue;

bool value_is_int(Value v);
bool value_is_symbol(Value v);
bool value_is_string(Value v);
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

Value car(Value v);
Value cdr(Value v);

void display(Value v);
Value parse(const char *path);
Value load(const char *path);
Value eval(Value v);
Value eval_string(const char *s);

ATTR_MALLOC char *stringify(Value v);
Value parse_string(const char *in);

const char *error_message(void);

#endif
