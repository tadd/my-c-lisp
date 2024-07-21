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
        assert_int_eq(length(exp), length(act)); \
        for (; exp != Qnil; exp = cdr(exp), act = cdr(act)) \
            assert_vint_eq(value_to_int(car(exp)), car(act)); \
    } while (0)
#define V(x) \
    _Generic(x, int: value_of_int(x), const char *: value_of_symbol, Value: x)

#define assert_int_eq(expected, actual) cr_assert(eq(int, expected, actual))
#define assert_vint_eq(expected, actual) do { \
        cr_assert(value_is_int(actual)); \
        assert_int_eq(expected, value_to_int(actual)); \
    } while (0)

#define assert_vtrue(actual) assert_int_eq(Qtrue, actual)
#define assert_vfalse(actual) assert_int_eq(Qfalse, actual)

#define assert_runtime_error(v, pattern) do { \
        assert_int_eq(Qundef, v); \
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

    //assert_stringify("<function>", value_of_cfunc(value_of_cfunc, 1));

    assert_stringify("(1)", cons(V(1), Qnil));
    assert_stringify("(1 . 2)", cons(V(1), V(2)));
    assert_stringify("(1 2)", list(V(1), V(2), Qundef));
}

Test(lisp, parse_int) {
    Value v = parse_expr_string("42");
    assert_vint_eq(42, v);

    v = parse_expr_string("-42");
    assert_vint_eq(-42, v);
    cr_assert_str_eq("-42", stringify(v));
}

Test(lisp, parse_nil) {
    Value v = parse_expr_string("()");
    cr_assert(value_is_nil(v));
}

Test(lisp, parse_list) {
    Value v = parse_expr_string("(1 2)");
    assert_vint_eq(1, car(v));
    assert_vint_eq(2, cadr(v));
    cr_assert(value_is_nil(cddr(v)));
}

Test(lisp, cxr) {
    Value v = parse_expr_string("((((42))))");
    Value i = caaaar(v);
    assert_vint_eq(42, i);
}

Test(lisp, parse_ident) {
    Value v = parse_expr_string("a");
    cr_assert(value_is_symbol(v));
    cr_assert_str_eq("a", value_to_string(v));
}

Test(lisp, parse_dot) {
    Value v2 = parse_expr_string("(1 . 2)");
    cr_assert(value_is_pair(v2));
    assert_vint_eq(1, car(v2));
    assert_vint_eq(2, cdr(v2));
}

Test(lisp, parse_peculiar) {
    Value v = parse_expr_string("+42");
    cr_assert(value_is_int(v));

    v = parse_expr_string("+");
    cr_assert(value_is_symbol(v));
}

Test(lisp, eval_arithmetic_literal) {
    Value v = eval_string("(+ 42 21)");
    assert_vint_eq(63, v);

    v = eval_string("(- 42 21)");
    assert_vint_eq(21, v);

    v = eval_string("(* 4 2)");
    assert_vint_eq(8, v);

    v = eval_string("(/ 4 2)");
    assert_vint_eq(2, v);
}

Test(lisp, eval_arithmetic_expr) {
    Value v = eval_string("(+ (+ 40 2) 21)");
    assert_vint_eq(63, v);

    v = eval_string("(+ (- 40 4) (* 3 (/ 100 50)))");
    assert_vint_eq(42, v);
}

Test(lisp, div0) {
    Value v = eval_string("(/ 42 0)");
    assert_runtime_error(v, "divided by zero");
}

Test(lisp, relop) {
    Value v;
    v = eval_string("(= 42 42)");
    assert_vtrue(v);
    v = eval_string("(= 0 0 0 0 0)");
    assert_vtrue(v);

    v = eval_string("(= 42 0)");
    assert_vfalse(v);
    v = eval_string("(= 0 0 0 0 42)");
    assert_vfalse(v);

    v = eval_string("(< 2 4)");
    assert_vtrue(v);
    v = eval_string("(< 2 3 4 5)");
    assert_vtrue(v);

    v = eval_string("(< 2 0)");
    assert_vfalse(v);
    v = eval_string("(< 2 3 4 4)");
    assert_vfalse(v);

    v = eval_string("(<= 2 4)");
    assert_vtrue(v);
    v = eval_string("(<= 2 3 4 4)");
    assert_vtrue(v);

    v = eval_string("(<= 2 0)");
    assert_vfalse(v);
    v = eval_string("(<= 2 3 4 3)");
    assert_vfalse(v);

    v = eval_string("(> 3 2)");
    assert_vtrue(v);
    v = eval_string("(> 4 3 2 1)");
    assert_vtrue(v);

    v = eval_string("(> 0 1)");
    assert_vfalse(v);
    v = eval_string("(> 4 3 2 2)");
    assert_vfalse(v);

    v = eval_string("(>= 3 2)");
    assert_vtrue(v);
    v = eval_string("(>= 4 3 2 2)");
    assert_vtrue(v);

    v = eval_string("(>= 0 1)");
    assert_vfalse(v);
    v = eval_string("(>= 4 3 2 3)");
    assert_vfalse(v);
}

Test(lisp, unbound_variable) {
    Value v = eval_string("x");
    assert_runtime_error(v, "unbound variable: x");

    v = eval_string("(+ x 2)");
    assert_runtime_error(v, "unbound variable: x");
}

Test(lisp, true_false) {
    Value v = eval_string("#t");
    assert_int_eq(v, Qtrue);
    v = eval_string("#f");
    assert_int_eq(v, Qfalse);
}

Test(lisp, if) {
    Value v;
    v = eval_string("(if #t 1)");
    assert_vint_eq(1, v);

    v = eval_string("(if #t 1 2)");
    assert_vint_eq(1, v);

    v = eval_string("(if #f 1 2)");
    assert_vint_eq(2, v);

    v = eval_string("(if #f)");
    assert_runtime_error(v, "2..3 but got 1");

    v = eval_string("(if #f 1 2 3)");
    assert_runtime_error(v, "2..3 but got 4");
}

Test(lisp, if_composed) {
    Value v;
    v = eval_string("(if (if #t 1 #f) (if #t 3 4) (if #t 5 6))");
    assert_vint_eq(3, v);

    v = eval_string("(if (if #f 1 #f) (if #f 3 4) (if #f 5 6))");
    assert_vint_eq(6, v);
}

Test(lisp, list) {
    Value v;
    v = eval_string("(list)");
    assert_int_eq(v, Qnil);

    v = eval_string("(list 42)");
    cr_assert(value_is_pair(v));
    assert_int_eq(1, length(v));
    assert_vint_eq(42, car(v));

    v = list(value_of_int(42),
             value_of_symbol("foo"),
             Qundef);
    cr_assert(value_is_pair(v));
    assert_int_eq(2, length(v));
    Value v0 = car(v);
    assert_vint_eq(42, v0);
    Value v1 = cadr(v);
    cr_assert(value_is_symbol(v1));
    cr_assert_str_eq("foo", value_to_string(v1));
}


Test(lisp, reverse) {
    Value v;
    v = eval_string("()");
    assert_list_eq(Qnil, v);
    v = eval_string("(reverse (list 1))");
    assert_list_eq(list(V(1), Qundef), v);
    v = eval_string("(reverse (list 1 2))");
    assert_list_eq(list(V(2), V(1), Qundef), v);
    v = eval_string("(reverse (list 1 2 3))");
    assert_list_eq(list(V(3), V(2), V(1), Qundef), v);
}

Test(lisp, define_variable) {
    Value v;
    v = eval_string("(define x 42) x");
    assert_vint_eq(42, v);

    v = eval_string("(define x (* -1 42)) x");
    assert_vint_eq(-42, v);
}

Test(lisp, set) {
    Value v;
    v = eval_string("(define x 1) (set! x 42) x");
    assert_vint_eq(42, v);

    v = eval_string("(set! x 42) x");
    assert_runtime_error(v, "unbound variable: x");
}

Test(lisp, let) {
    Value v;
    v = eval_string("(let ((x 42)) x)");
    assert_vint_eq(42, v);

    v = eval_string("(let ((x 42) (y 21)) (+ x y))");
    assert_vint_eq(63, v);

    v = eval_string("(let ((x 42)) (let ((y 21)) (+ x y)))");
    assert_vint_eq(63, v);

    v = eval_string("(let ((x 42)) (let ((x 1)) x))");
    assert_vint_eq(1, v);

    v = eval_string("(let ((x 42)) (let ((y x)) y))");
    assert_vint_eq(42, v);

    v = eval_string("(let ((x 42)) (let ((x x)) x))");
    assert_vint_eq(42, v);

    v = eval_string("(let ((x 42)) (let ((x 10)) x) x)");
    assert_vint_eq(42, v);

    v = eval_string("(let ((x 42) (y 10)) (list x y))");
    assert_list_eq(list(V(42), V(10), Qundef), v);

    v = eval_string("(let ((x 42)))");
    assert_runtime_error(v, "one or more expressions");

    v = eval_string("(let ((x 42) (y 100)))");
    assert_runtime_error(v, "one or more expressions");
}

Test(lisp, let_body_define) {
    Value v;
    v = eval_string("(let ((x 42)) (define x 2) x)");
    assert_vint_eq(2, v);

    v = eval_string("(define x 1) (let ((x 42)) (define x 2) x)");
    assert_vint_eq(2, v);

    v = eval_string("(define x 1) (let () (define x 2) x) x");
    assert_vint_eq(1, v);
}

Test(lisp, let_star) {
    Value v;
    v = eval_string("(let* ((x 42) (y 10)) (list x y))");
    assert_list_eq(list(V(42), V(10), Qundef), v);
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
    assert_vint_eq(1, v);

    v = eval_string("((lambda (x) (* 2 x)) 21)");
    assert_vint_eq(42, v);

    v = eval_string("((lambda (x y) (* x y)) 3 14)");
    assert_vint_eq(42, v);

    v = eval_string("(define mul (lambda (x y) (* x y))) (mul 3 14)");
    assert_vint_eq(42, v);
}

Test(lisp, begin) {
    Value v;
    v = eval_string("(begin 1 2 3)");
    assert_vint_eq(3, v);
}

Test(lisp, cond) {
    Value v;
    v = eval_string("(cond (#f 1) (#t 2) (else 3))");
    assert_vint_eq(2, v);

    v = eval_string("(cond (#f 1) (else 3))");
    assert_vint_eq(3, v);

    v = eval_string("(cond (#f) (2))");
    assert_vint_eq(2, v);
}
