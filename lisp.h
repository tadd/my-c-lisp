#ifndef LISP_H
#define LISP_H

#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>

#include "utils.h"

typedef struct Pair Pair;
typedef union {
    Pair *pair;
    uintptr_t raw;
} Value;

bool value_is_int(Value v);
bool value_is_symbol(Value v ATTR_UNUSED);
bool value_is_atom(Value v);
bool value_is_pair(Value v);
bool value_is_nil(Value v);
int64_t value_to_int(Value v);
Value value_of_int(int64_t i);

void print(Value v);
Value parse(FILE *in);
Value eval(Value v);

#endif
