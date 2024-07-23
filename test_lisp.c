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
#define assert_str_list_eq(expected, actual) do { \
        Value exp = expected, act = actual; \
        cr_assert(value_is_pair(act)); \
        assert_int_eq(length(exp), length(act)); \
        for (; exp != Qnil; exp = cdr(exp), act = cdr(act)) \
            cr_assert_str_eq(value_to_string(car(exp)), value_to_string(car(act))); \
    } while (0)

static inline Value value_idfunc(Value x) { return x; }
#define V(x) \
    _Generic(x, int: value_of_int, char *: value_of_string, Value: value_idfunc)(x)

#define assert_int_eq(expected, actual) cr_assert(eq(int, expected, actual))
#define assert_vint_eq(expected, actual) do { \
        cr_assert(value_is_int(actual)); \
        assert_int_eq(expected, value_to_int(actual)); \
    } while (0)

#define assert_vstr_eq(expected, actual) do { \
        cr_assert(value_is_string(actual)); \
        cr_assert_str_eq(expected, value_to_string(actual)); \
    } while (0)

#define assert_vtrue(actual) assert_int_eq(Qtrue, actual)
#define assert_vfalse(actual) assert_int_eq(Qfalse, actual)

#define assert_runtime_error(v, pattern) do { \
        assert_int_eq(Qundef, v); \
        cr_assert_not_null(strstr(error_message(), pattern)); \
    } while (0)

#define assert_vint_eq_parsed(expected, actual) \
    assert_vint_eq(expected, parse_expr_string(actual))
#define assert_vstr_eq_parsed(expected, actual) \
    assert_vstr_eq(expected, parse_expr_string(actual))

#define assert_int_eq_evaled(expected, actual) \
    assert_int_eq(expected, eval_string(actual))
#define assert_vint_eq_evaled(expected, actual) \
    assert_vint_eq(expected, eval_string(actual))
#define assert_runtime_error_evaled(expected, actual) \
    assert_runtime_error(eval_string(actual), expected)
#define assert_vtrue_evaled(actual) assert_vtrue(eval_string(actual))
#define assert_vfalse_evaled(actual) assert_vfalse(eval_string(actual))
#define assert_list_eq_evaled(expected, actual) \
    assert_list_eq(expected, eval_string(actual))


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

    assert_stringify("(1)", cons(V(1), Qnil));
    assert_stringify("(1 . 2)", cons(V(1), V(2)));
    assert_stringify("(1 2)", list(V(1), V(2), Qundef));
}

Test(lisp, parse_int) {
    assert_vint_eq_parsed(42, "42");
    assert_vint_eq_parsed(-42, "-42");
}

Test(lisp, parse_nil) {
    cr_assert(value_is_nil(parse_expr_string("()")));
}

Test(lisp, parse_list) {
    assert_list_eq(list(V(1), V(2), Qundef), parse_expr_string("(1 2)"));
}

Test(lisp, parse_string) {
    assert_vstr_eq_parsed("abc", "\"abc\"");
    assert_vstr_eq_parsed("a\\b", "\"a\\\\b\"");
    assert_vstr_eq_parsed("a\"b", "\"a\\\"b\"");
}

Test(lisp, parse_string_list) {
    Value v = parse_expr_string("(\"abc\" \"def\")");
    cr_assert(value_is_pair(v));
    cr_assert(not(value_is_nil(v)));

    assert_str_list_eq(list(V("abc"), V("def"), Qundef), v);
}

Test(lisp, cxr) {
    assert_vint_eq(42, caaaar(parse_expr_string("((((42))))")));
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
    assert_vint_eq_parsed(42, "+42");

    cr_assert(value_is_symbol(parse_expr_string("+")));
}

Test(lisp, eval_arithmetic_literal) {
    assert_vint_eq_evaled(63, "(+ 42 21)");
    assert_vint_eq_evaled(21, "(- 42 21)");
    assert_vint_eq_evaled(8, "(* 4 2)");
    assert_vint_eq_evaled(2, "(/ 4 2)");
}

Test(lisp, eval_arithmetic_expr) {
    assert_vint_eq_evaled(63, "(+ (+ 40 2) 21)");
    assert_vint_eq_evaled(42, "(+ (- 40 4) (* 3 (/ 100 50)))");
}

Test(lisp, div0) {
    assert_runtime_error_evaled("divided by zero", "(/ 42 0)");
}

Test(lisp, relop) {
    assert_vtrue_evaled("(= 42 42)");
    assert_vtrue_evaled("(= 0 0 0 0 0)");

    assert_vfalse_evaled("(= 42 0)");
    assert_vfalse_evaled("(= 0 0 0 0 42)");

    assert_vtrue_evaled("(< 2 4)");
    assert_vtrue_evaled("(< 2 3 4 5)");

    assert_vfalse_evaled("(< 2 0)");
    assert_vfalse_evaled("(< 2 3 4 4)");

    assert_vtrue_evaled("(<= 2 4)");
    assert_vtrue_evaled("(<= 2 3 4 4)");

    assert_vfalse_evaled("(<= 2 0)");
    assert_vfalse_evaled("(<= 2 3 4 3)");

    assert_vtrue_evaled("(> 3 2)");
    assert_vtrue_evaled("(> 4 3 2 1)");

    assert_vfalse_evaled("(> 0 1)");
    assert_vfalse_evaled("(> 4 3 2 2)");

    assert_vtrue_evaled("(>= 3 2)");
    assert_vtrue_evaled("(>= 4 3 2 2)");

    assert_vfalse_evaled("(>= 0 1)");
    assert_vfalse_evaled("(>= 4 3 2 3)");
}

Test(lisp, modulo) {
    assert_vint_eq_evaled(1, "(modulo 13 4)");
    assert_vint_eq_evaled(3, "(modulo -13 4)");
    assert_vint_eq_evaled(-3, "(modulo 13 -4)");
    assert_vint_eq_evaled(-1, "(modulo -13 -4)");

    assert_runtime_error_evaled("divided by zero", "(modulo 13 0)");
}

Test(lisp, unbound_variable) {
    assert_runtime_error_evaled("unbound variable: x", "x");
    assert_runtime_error_evaled("unbound variable: x", "(+ x 2)");
}

Test(lisp, true_false) {
    assert_int_eq_evaled(Qtrue, "#t");
    assert_int_eq_evaled(Qfalse, "#f");
}

Test(lisp, if) {
    assert_vint_eq_evaled(1, "(if #t 1)");
    assert_vint_eq_evaled(1, "(if #t 1 2)");
    assert_vint_eq_evaled(2, "(if #f 1 2)");

    assert_runtime_error_evaled("2..3 but got 1", "(if #f)");
    assert_runtime_error_evaled("2..3 but got 4", "(if #f 1 2 3)");
}

Test(lisp, if_composed) {
    assert_vint_eq_evaled(3, "(if (if #t 1 #f) (if #t 3 4) (if #t 5 6))");
    assert_vint_eq_evaled(6, "(if (if #f 1 #f) (if #f 3 4) (if #f 5 6))");
}

Test(lisp, list) {
    assert_int_eq_evaled(Qnil, "()");
    assert_int_eq_evaled(Qnil, "(list)");

    assert_list_eq_evaled(list(V(42), Qundef), "(list 42)");

    Value v = list(V(42), V("foo"), Qundef);
    cr_assert(value_is_pair(v));
    assert_int_eq(2, length(v));
    assert_vint_eq(42, car(v));
    assert_vstr_eq("foo", cadr(v));
}


Test(lisp, reverse) {
    assert_list_eq_evaled(Qnil, "()");
    assert_list_eq_evaled(list(V(1), Qundef), "(reverse (list 1))");
    assert_list_eq_evaled(list(V(2), V(1), Qundef), "(reverse (list 1 2))");
    assert_list_eq_evaled(list(V(3), V(2), V(1), Qundef), "(reverse (list 1 2 3))");
}

Test(lisp, cons_etc) {
    Value v = eval_string("(cons 1 2)");
    cr_assert(value_is_pair(v));
    assert_vint_eq(1, car(v));
    assert_vint_eq(2, cdr(v));

    assert_vint_eq_evaled(1, "(car (cons 1 2))");
    assert_vint_eq_evaled(2, "(cdr (cons 1 2))");
}

Test(lisp, define_variable) {
    assert_vint_eq_evaled(42, "(define x 42) x");
    assert_vint_eq_evaled(-42, "(define x (* -1 42)) x");
}

Test(lisp, set) {
    assert_vint_eq_evaled(42, "(define x 1) (set! x 42) x");
    assert_runtime_error_evaled("unbound variable: x", "(set! x 42) x");
}

Test(lisp, let) {
    assert_vint_eq_evaled(42, "(let ((x 42)) x)");
    assert_vint_eq_evaled(63, "(let ((x 42) (y 21)) (+ x y))");
    assert_vint_eq_evaled(63, "(let ((x 42)) (let ((y 21)) (+ x y)))");
    assert_vint_eq_evaled(1, "(let ((x 42)) (let ((x 1)) x))");
    assert_vint_eq_evaled(42, "(let ((x 42)) (let ((y x)) y))");
    assert_vint_eq_evaled(42, "(let ((x 42)) (let ((x x)) x))");
    assert_vint_eq_evaled(42, "(let ((x 42)) (let ((x 10)) x) x)");
    assert_list_eq_evaled(list(V(42), V(10), Qundef), "(let ((x 42) (y 10)) (list x y))");

    assert_runtime_error_evaled("one or more expressions", "(let ((x 42)))");
    assert_runtime_error_evaled("one or more expressions", "(let ((x 42) (y 100)))");
}

Test(lisp, let_body_define) {
    assert_vint_eq_evaled(2, "(let ((x 42)) (define x 2) x)");
    assert_vint_eq_evaled(2, "(define x 1) (let ((x 42)) (define x 2) x)");
    assert_vint_eq_evaled(1, "(define x 1) (let () (define x 2) x) x");
}

Test(lisp, let_star) {
    assert_list_eq_evaled(list(V(42), V(10), Qundef),
                          "(let* ((x 42) (y 10)) (list x y))");
}

Test(lisp, letrec) {
    assert_vtrue_evaled(
"(letrec ((myeven?\n"
"          (lambda (n)\n"
"            (if (= n 0)\n"
"                #t\n"
"                (myodd? (- n 1)))))\n"
"         (myodd?\n"
"          (lambda (n)\n"
"            (if (= n 0)\n"
"                #f\n"
"                (myeven? (- n 1))))))\n"
"   (myeven? 8888))");
}

Test(lisp, applicable) {
    assert_runtime_error_evaled("expected applicative", "(1 1)");
    assert_runtime_error_evaled("expected applicative", "(() 1)");
}

Test(lisp, lambda) {
    cr_assert(value_is_closure(eval_string("(lambda () 1)")));

    assert_vint_eq_evaled(1, "((lambda () 1))");
    assert_vint_eq_evaled(42, "((lambda (x) (* 2 x)) 21)");
    assert_vint_eq_evaled(42, "((lambda (x y) (* x y)) 3 14)");
    assert_vint_eq_evaled(42, "(define mul (lambda (x y) (* x y))) (mul 3 14)");
}

Test(lisp, lambda_rec) {
    assert_vint_eq_evaled(1, "(define f (lambda (x) (if (> x 0) x (f (+ x 1))))) (f 0)");
}

Test(lisp, begin) {
    assert_vint_eq_evaled(3, "(begin 1 2 3)");
}

Test(lisp, cond) {
    assert_vint_eq_evaled(2, "(cond (#f 1) (#t 2) (else 3))");
    assert_vint_eq_evaled(3, "(cond (#f 1) (else 3))");
    assert_vint_eq_evaled(2, "(cond (#f) (2))");
}

Test(lisp, cputime) {
    Value v = eval_string("(_cputime)");
    cr_assert(value_is_int(v));
    cr_assert(gt(int, value_to_int(v), 0));
}

Test(lisp, comment) {
    assert_vint_eq_evaled(1, "1 ; foo");
    assert_vint_eq_evaled(2, "2 ;;bar");
    assert_list_eq_evaled(list(V(1), V(2), Qundef), "(list 1 ;;; ?? ;;; \n 2)");
}

Test(lisp, define_and_lambda) {
    assert_vint_eq_evaled(42,
"(define f (lambda () (g)))"
"(define g (lambda () 42))"
"(f)");
}
