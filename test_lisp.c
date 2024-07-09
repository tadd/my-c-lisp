#include <stdio.h>
#include <stdlib.h>

#include <criterion/criterion.h>
#include <criterion/new/assert.h>

#include "lisp.h"

Test(lisp, nil) {
    Value a = VALUE_NIL;
    cr_assert(value_is_nil(a));
}

Test(lisp, parse_int) {
    Value v = parse_expr_from_string("42");
    cr_assert(eq(42, value_to_int(v)));
}

Test(lisp, parse_nil) {
    Value v = parse_expr_from_string("()");
    cr_assert(value_is_nil(v));
}

Test(lisp, parse_list) {
    Value v = parse_expr_from_string("(1 2)");
    cr_assert(eq(1, value_to_int(car(v))));
    cr_assert(eq(2, value_to_int(car(cdr(v)))));
    cr_assert(value_is_nil(cdr(cdr(v))));
}
