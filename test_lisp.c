#include <stdio.h>
#include <stdlib.h>

#include <criterion/criterion.h>
#include <criterion/new/assert.h>

#include "lisp.h"

#define assert_stringify(expected, v) do { \
        char *s = stringify(v); \
        cr_assert_str_eq(expected, s); \
        free(s); \
    } while (0)

#define assert_list_eq(expected, actual) do { \
        Value exp = expected, act = actual; \
        cr_assert(value_is_pair(act)); \
        assert_eq(length(exp), length(act)); \
        for (; exp != Qnil; exp = cdr(exp), act = cdr(act)) \
            assert_eq(value_to_int(car(exp)), value_to_int(car(act))); \
    } while (0)

#define assert_eq(expected, actual) cr_assert(eq(int, expected, actual))

#define V(x) \
    _Generic(x, int: value_of_int(x), const char *: value_of_symbol, Value: x)

#define assert_runtime_error(v, pattern) do { \
        assert_eq(Qundef, v); \
        cr_assert_not_null(strstr(error_message(), pattern)); \
    } while (0)

Test(lisp, nil) {
    Value a = Qnil;
    cr_assert(value_is_nil(a));
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

    assert_stringify("<function>", value_of_cfunc(value_of_cfunc, 1));

    assert_stringify("(1)", cons(value_of_int(1), Qnil));
    assert_stringify("(1 . 2)", cons(value_of_int(1), value_of_int(2)));
    assert_stringify("(1 2)", cons(value_of_int(1), cons(value_of_int(2), Qnil)));
}

Test(lisp, parse_int) {
    Value v = parse_expr_string("42");
    assert_eq(42, value_to_int(v));

    v = parse_expr_string("-42");
    assert_eq(-42, value_to_int(v));
    cr_assert_str_eq("-42", stringify(v));
}

Test(lisp, parse_nil) {
    Value v = parse_expr_string("()");
    cr_assert(value_is_nil(v));
}

Test(lisp, parse_list) {
    Value v = parse_expr_string("(1 2)");
    assert_eq(1, value_to_int(car(v)));
    assert_eq(2, value_to_int(cadr(v)));
    cr_assert(value_is_nil(cddr(v)));
}

Test(lisp, cxr) {
    Value v = parse_expr_string("((((42))))");
    Value i = caaaar(v);
    cr_assert(value_is_int(i));
    assert_eq(42, value_to_int(i));
}

Test(lisp, parse_ident) {
    Value v = parse_expr_string("a");
    cr_assert(value_is_symbol(v));
    cr_assert_str_eq("a", value_to_string(v));
}

Test(lisp, parse_dot) {
    Value v2 = parse_expr_string("(1 . 2)");
    cr_assert(value_is_pair(v2));
    assert_eq(1, value_to_int(car(v2)));
    assert_eq(2, value_to_int(cdr(v2)));
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
    assert_eq(63, value_to_int(v));

    v = eval_string("(- 42 21)");
    cr_assert(value_is_int(v));
    assert_eq(21, value_to_int(v));

    v = eval_string("(* 4 2)");
    cr_assert(value_is_int(v));
    assert_eq(8, value_to_int(v));

    v = eval_string("(/ 4 2)");
    cr_assert(value_is_int(v));
    assert_eq(2, value_to_int(v));
}

Test(lisp, eval_arithmetic_expr) {
    Value v = eval_string("(+ (+ 40 2) 21)");
    cr_assert(value_is_int(v));
    assert_eq(63, value_to_int(v));

    v = eval_string("(+ (- 40 4) (* 3 (/ 100 50)))");
    cr_assert(value_is_int(v));
    assert_eq(42, value_to_int(v));
}

Test(lisp, div0) {
    Value v = eval_string("(/ 42 0)");
    assert_runtime_error(v, "divided by zero");
}

Test(lisp, unbound_variable) {
    Value v = eval_string("x");
    assert_runtime_error(v, "unbound variable: x");

    v = eval_string("(+ x 2)");
    assert_runtime_error(v, "unbound variable: x");
}

Test(lisp, true_false) {
    Value v = eval_string("#t");
    assert_eq(v, Qtrue);
    v = eval_string("#f");
    assert_eq(v, Qfalse);
}

Test(lisp, if) {
    Value v;
    v = eval_string("(if #t 1)");
    assert_eq(1, value_to_int(v));

    v = eval_string("(if #t 1 2)");
    assert_eq(1, value_to_int(v));

    v = eval_string("(if #f 1 2)");
    assert_eq(2, value_to_int(v));

    v = eval_string("(if #f)");
    assert_runtime_error(v, "2..3 but got 1");

    v = eval_string("(if #f 1 2 3)");
    assert_runtime_error(v, "2..3 but got 4");
}

Test(lisp, if_composed) {
    Value v;
    v = eval_string("(if (if #t 1 #f) (if #t 3 4) (if #t 5 6))");
    assert_eq(3, value_to_int(v));

    v = eval_string("(if (if #f 1 #f) (if #f 3 4) (if #f 5 6))");
    assert_eq(6, value_to_int(v));
}

Test(lisp, list) {
    Value v;
    v = list(Qundef);
    assert_eq(v, Qnil);

    v = list(value_of_int(42), Qundef);
    cr_assert(value_is_pair(v));
    assert_eq(1, length(v));
    cr_assert(value_is_int(car(v)));
    assert_eq(42, value_to_int(car(v)));

    v = list(value_of_int(42),
             value_of_symbol("foo"),
             value_of_cfunc(value_of_cfunc, 0),
             Qundef);
    cr_assert(value_is_pair(v));
    assert_eq(3, length(v));
    Value v0 = car(v);
    cr_assert(value_is_int(v0));
    assert_eq(42, value_to_int(v0));
    Value v1 = cadr(v);
    cr_assert(value_is_symbol(v1));
    cr_assert_str_eq("foo", value_to_string(v1));
    Value v2 = caddr(v);
    cr_assert(value_is_cfunc(v2));
    cr_assert_str_eq("<function>", stringify(v2));
}


Test(lisp, reverse) {
    assert_list_eq(Qnil, reverse(Qnil));
    Value l;
    l = list(V(1), Qundef);
    assert_list_eq(l, reverse(l));
    assert_list_eq(list(V(2), V(1), Qundef), reverse(list(V(1), V(2), Qundef)));
    assert_list_eq(list(V(3), V(2), V(1), Qundef),
                   reverse(list(V(1), V(2), V(3), Qundef)));
}

Test(lisp, define_variable) {
    Value v;
    v = eval_string("(define x 42) x");
    cr_assert(value_is_int(v));
    assert_eq(42, value_to_int(v));

    v = eval_string("(define x (* -1 42)) x");
    cr_assert(value_is_int(v));
    assert_eq(-42, value_to_int(v));
}

Test(lisp, set) {
    Value v;
    v = eval_string("(define x 1) (set! x 42) x");
    cr_assert(value_is_int(v));
    assert_eq(42, value_to_int(v));

    v = eval_string("(set! x 42) x");
    assert_runtime_error(v, "unbound variable: x");
}

Test(lisp, let) {
    Value v;
    v = eval_string("(let ((x 42)) x)");
    cr_assert(value_is_int(v));
    assert_eq(42, value_to_int(v));

    v = eval_string("(let ((x 42) (y 21)) (+ x y))");
    cr_assert(value_is_int(v));
    assert_eq(63, value_to_int(v));

    v = eval_string("(let ((x 42)) (let ((y 21)) (+ x y)))");
    cr_assert(value_is_int(v));
    assert_eq(63, value_to_int(v));

    v = eval_string("(let ((x 42)) (let ((x 1)) x))");
    cr_assert(value_is_int(v));
    assert_eq(1, value_to_int(v));

    v = eval_string("(let ((x 42)) (let ((y x)) y))");
    cr_assert(value_is_int(v));
    assert_eq(42, value_to_int(v));

    v = eval_string("(let ((x 42)) (let ((x x)) x))");
    cr_assert(value_is_int(v));
    assert_eq(42, value_to_int(v));

    v = eval_string("(let ((x 42)) (let ((x 10)) x) x)");
    cr_assert(value_is_int(v));
    assert_eq(42, value_to_int(v));

    v = eval_string("(let ((x 42) (y 10)) (list x y))");
    cr_assert(value_is_pair(v));
    assert_eq(42, value_to_int(car(v)));
    assert_eq(10, value_to_int(cadr(v)));

    v = eval_string("(let ((x 42)))");
    assert_runtime_error(v, "one or more expressions");

    v = eval_string("(let ((x 42) (y 100)))");
    assert_runtime_error(v, "one or more expressions");
}

Test(lisp, let_body_define) {
    Value v;
    v = eval_string("(let ((x 42)) (define x 2) x)");
    cr_assert(value_is_int(v));
    assert_eq(2, value_to_int(v));

    v = eval_string("(define x 1) (let ((x 42)) (define x 2) x)");
    cr_assert(value_is_int(v));
    assert_eq(2, value_to_int(v));

    v = eval_string("(define x 1) (let () (define x 2) x) x");
    cr_assert(value_is_int(v));
    assert_eq(1, value_to_int(v));
}

Test(lisp, let_star) {
    Value v;
    v = eval_string("(let* ((x 42) (y 10)) (list x y))");
    cr_assert(value_is_pair(v));
    assert_eq(42, value_to_int(car(v)));
    assert_eq(10, value_to_int(cadr(v)));
}

Test(lisp, applicable) {
    Value v;
    v = eval_string("(1 1)");
    assert_runtime_error(v, "expected applicative");
}

Test(lisp, lambda) {
    Value v;
    v = eval_string("(lambda () 1)");
    cr_assert(value_is_closure(v));

    v = eval_string("((lambda () 1))");
    cr_assert(value_is_int(v));
    assert_eq(1, value_to_int(v));

    v = eval_string("((lambda (x) (* 2 x)) 21)");
    cr_assert(value_is_int(v));
    assert_eq(42, value_to_int(v));

    v = eval_string("((lambda (x y) (* x y)) 3 14)");
    cr_assert(value_is_int(v));
    assert_eq(42, value_to_int(v));
}
