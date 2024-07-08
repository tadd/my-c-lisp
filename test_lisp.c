#include <stdio.h>
#include <stdlib.h>

#include <criterion/criterion.h>
#include <criterion/new/assert.h>

#include "lisp.h"

Test(lisp, nil) {
    Value a = VALUE_NIL;
    cr_assert(value_is_nil(a));
}

Test(lisp, parse) {
    Value a = parse_expr_from_string("42");
    cr_assert(eq(42, value_to_int(a)));
}
