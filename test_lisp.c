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
static void assert_stringify(const char *expected, Value v)
{
        char *s = stringify(v);
        cr_assert(streq(expected, s));
        free(s);
}

Test(lisp, printing) {
    assert_stringify("#t", Qtrue);
    assert_stringify("#f", Qfalse);
    assert_stringify("<undef>", Qundef);
    assert_stringify("()", Qnil);

    assert_stringify("0", value_of_int(0));
    assert_stringify("42", value_of_int(42));
    assert_stringify("-42", value_of_int(-42));

    assert_stringify("'foo", value_of_symbol("foo"));

    assert_stringify("\"bar\"", value_of_string("bar"));
    assert_stringify("\"\\\"", value_of_string("\\"));

    assert_stringify("<function>", value_of_func(value_of_func, 1));

    assert_stringify("(1)", cons(value_of_int(1), Qnil));
    assert_stringify("(1 . 2)", cons(value_of_int(1), value_of_int(2)));
    assert_stringify("(1 2)", cons(value_of_int(1), cons(value_of_int(2), Qnil)));
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

    v = eval_string("(+ x 2)");
    cr_assert(eq(v, Qundef));
}

Test(lisp, true_false) {
    Value v = eval_string("#t");
    cr_assert(eq(v, Qtrue));
    v = eval_string("#f");
    cr_assert(eq(v, Qfalse));
}

Test(lisp, if) {
    Value v;
    v = eval_string("(if #t 1)");
    cr_assert(eq(1, value_to_int(v)));

#if 0 // undefined in R^5RS, maybe
    v = eval_string("(if #f 1)");
    cr_assert(eq(Qfalse, v));
#endif

    v = eval_string("(if #t 1 2)");
    cr_assert(eq(1, value_to_int(v)));

    v = eval_string("(if #f 1 2)");
    cr_assert(eq(2, value_to_int(v)));
}

Test(lisp, if_composed) {
    Value v;
    v = eval_string("(if (if #t 1 #f) (if #t 3 4) (if #t 5 6))");
    cr_assert(eq(3, value_to_int(v)));

    v = eval_string("(if (if #f 1 #f) (if #f 3 4) (if #f 5 6))");
    cr_assert(eq(6, value_to_int(v)));
}

Test(lisp, list) {
    Value v;
    v = list(Qundef);
    cr_assert(eq(v, Qnil));

    v = list(value_of_int(42), Qundef);
    cr_assert(value_is_pair(v));
    cr_assert(eq(1, length(v)));
    cr_assert(value_is_int(car(v)));
    cr_assert(eq(42, value_to_int(car(v))));

    v = list(value_of_int(42),
             value_of_string("foo"),
             value_of_func(value_of_func, 0),
             Qundef);
    cr_assert(value_is_pair(v));
    cr_assert(eq(3, length(v)));
    Value v0 = car(v);
    cr_assert(value_is_int(v0));
    cr_assert(eq(42, value_to_int(v0)));
    Value v1 = cadr(v);
    cr_assert(value_is_string(v1));
    cr_assert(streq("foo", value_to_string(v1)));
    Value v2 = caddr(v);
    cr_assert(value_is_func(v2));
    cr_assert(streq("<function>", stringify(v2)));
}

Test(lisp, define_variable) {
    Value v;
    v = eval_string("(define x 42)");
    cr_assert(value_is_symbol(v));
    cr_assert(streq("x", value_to_string(v)));

    v = eval_string("x");
    cr_assert(value_is_int(v));
    cr_assert(eq(42, value_to_int(v)));

    v = eval_string("(define x (* -1 42)) x");
    cr_assert(value_is_int(v));
    cr_assert(eq(int, -42, value_to_int(v)));
}
