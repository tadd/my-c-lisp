#include <stdio.h>
#include <stdlib.h>

#include <criterion/criterion.h>
#include <criterion/new/assert.h>
#define streq(x, y) eq(str, (char *)x, (char *)y)

#include "lisp.h"

Test(lisp, nil) {
    Value a = Qnil;
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
    cr_assert(eq(2, value_to_int(cadr(v))));
    cr_assert(value_is_nil(cddr(v)));
}

Test(lisp, parse_string) {
    Value v = parse_expr_from_string("\"abc\"");
    cr_assert(value_is_string(v));
    cr_assert(streq("abc", value_to_string(v)));

    v = parse_expr_from_string("\"a\\\\b\"");
    cr_assert(value_is_string(v));
    cr_assert(streq("a\\b", value_to_string(v)));

    v = parse_expr_from_string("\"a\\\"b\"");
    cr_assert(value_is_string(v));
    cr_assert(streq("a\"b", value_to_string(v)));
}

Test(lisp, parse_string_list) {
    Value v = parse_expr_from_string("(\"abc\" \"def\")");
    cr_assert(value_is_pair(v));
    cr_assert(not(value_is_nil(v)));

    Value s = car(v);
    cr_assert(value_is_string(s));
    cr_assert(streq("abc", value_to_string(s)));

    Value t = car(cdr(v));
    cr_assert(value_is_string(t));
    cr_assert(streq("def", value_to_string(t)));
}
