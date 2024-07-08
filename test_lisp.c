#include <stdio.h>
#include <stdlib.h>

#include <criterion/criterion.h>
#include <criterion/new/assert.h>

#include "lisp.h"

Test(lisp, nil) {
    Value a = VALUE_NIL;
    cr_assert(value_is_nil(a));
}
