#ifndef SCHAF_H
#define SCHAF_H

#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>

#include "utils.h"

typedef enum {
// immediate
    SCH_TYPE_BOOL,
    SCH_TYPE_INT,
    SCH_TYPE_SYMBOL,
    SCH_TYPE_UNDEF,
// boxed (tagged)
    SCH_TYPE_PAIR,
    SCH_TYPE_STR,
    SCH_TYPE_PROC,
} SchType;

typedef uintptr_t Value;
typedef uintptr_t Symbol;

extern const Value Qnil, Qundef, Qfalse, Qtrue;

bool sch_value_is_int(Value v);
bool sch_value_is_symbol(Value v);
bool sch_value_is_string(Value v);
bool sch_value_is_pair(Value v);
SchType sch_value_type_of(Value v);

int64_t sch_value_to_int(Value v);
Symbol sch_value_to_symbol(Value v);
const char *sch_value_to_string(Value v);

Value sch_value_of_int(int64_t i);
Value sch_value_of_symbol(const char *s);
Value sch_value_of_string(const char *s);

Value cons(Value car, Value cdr);
int64_t length(Value list);

Value car(Value v);
Value cdr(Value v);

ATTR_MALLOC char *stringify(Value v);
void display(Value v);
Value parse(const char *path);
Value parse_string(const char *in);
Value load(const char *path);
Value eval_string(const char *s);

const char *error_message(void);

#endif
