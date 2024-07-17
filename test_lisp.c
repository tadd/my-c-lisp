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
    Value v = parse_expr_string("42");
    cr_assert(eq(42, value_to_int(v)));

    v = parse_expr_string("-42");
    cr_assert(eq(int, -42, value_to_int(v)));
    cr_assert(streq("-42", stringify(v)));
}

Test(lisp, parse_nil) {
    Value v = parse_expr_string("()");
    cr_assert(value_is_nil(v));
}

Test(lisp, parse_list) {
    Value v = parse_expr_string("(1 2)");
    cr_assert(eq(1, value_to_int(car(v))));
    cr_assert(eq(2, value_to_int(cadr(v))));
    cr_assert(value_is_nil(cddr(v)));
}

Test(lisp, parse_string) {
    Value v = parse_expr_string("\"abc\"");
    cr_assert(value_is_string(v));
    cr_assert(streq("abc", value_to_string(v)));

    v = parse_expr_string("\"a\\\\b\"");
    cr_assert(value_is_string(v));
    cr_assert(streq("a\\b", value_to_string(v)));

    v = parse_expr_string("\"a\\\"b\"");
    cr_assert(value_is_string(v));
    cr_assert(streq("a\"b", value_to_string(v)));
}

Test(lisp, parse_string_list) {
    Value v = parse_expr_string("(\"abc\" \"def\")");
    cr_assert(value_is_pair(v));
    cr_assert(not(value_is_nil(v)));

    Value s = car(v);
    cr_assert(value_is_string(s));
    cr_assert(streq("abc", value_to_string(s)));

    Value t = car(cdr(v));
    cr_assert(value_is_string(t));
    cr_assert(streq("def", value_to_string(t)));
}

Test(lisp, cxr) {
    Value v = parse_expr_string("((((42))))");
    Value i = caaaar(v);
    cr_assert(value_is_int(i));
    cr_assert(eq(42, value_to_int(i)));
}

Test(lisp, parse_ident) {
    Value v = parse_expr_string("a");
    cr_assert(value_is_symbol(v));
    cr_assert(streq("a", value_to_string(v)));
}

Test(lisp, parse_dotty) {
    Value v = parse_expr_string("...");
    cr_assert(value_is_symbol(v));
    cr_assert(streq("...", value_to_string(v)));

    Value v2 = parse_expr_string("(1 . 2)");
    cr_assert(value_is_pair(v2));
    cr_assert(eq(int, 1, value_to_int(car(v2))));
    cr_assert(eq(int, 2, value_to_int(cdr(v2))));
}

Test(lisp, parse_peculiar) {
    Value v = parse_expr_string("+42");
    cr_assert(value_is_int(v));

    v = parse_expr_string("+");
    cr_assert(value_is_symbol(v));
}

Test(lisp, eval_arithmetic_literal) {
    Value v = eval_string("(+ 42 21)");
    cr_assert(value_is_int(v));
    cr_assert(eq(int, 63, value_to_int(v)));

    v = eval_string("(- 42 21)");
    cr_assert(value_is_int(v));
    cr_assert(eq(int, 21, value_to_int(v)));

    v = eval_string("(* 4 2)");
    cr_assert(value_is_int(v));
    cr_assert(eq(int, 8, value_to_int(v)));

    v = eval_string("(/ 4 2)");
    cr_assert(value_is_int(v));
    cr_assert(eq(int, 2, value_to_int(v)));
}

Test(lisp, eval_arithmetic_expr) {
    Value v = eval_string("(+ (+ 40 2) 21)");
    cr_assert(value_is_int(v));
    cr_assert(eq(int, 63, value_to_int(v)));

    v = eval_string("(+ (- 40 4) (* 3 (/ 100 50)))");
    cr_assert(value_is_int(v));
    cr_assert(eq(int, 42, value_to_int(v)));
}

Test(lisp, unbound_variable) {
    Value v = eval_string("x");
    cr_assert(eq(v, Qundef));
}
