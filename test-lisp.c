#include <stdio.h>
#include <stdlib.h>

#include <criterion/criterion.h>
#include <criterion/new/assert.h>

#include "lisp.h"

#define assert_stringify(exp, v) do { \
        char *s = stringify(v); \
        cr_assert_str_eq(s, exp);   \
        free(s); \
    } while (0)

#define value_idfunc list
#define V(x) \
    _Generic(x, int: value_of_int, char *: value_of_string, Value: value_idfunc)(x)
#define Vsym(x) value_of_symbol(x)
#define assert_v_eq(exp, act)  do { \
        Value tmpe = exp, tmpa = act; \
        if (value_is_int(tmpe)) \
            assert_int_eq(tmpe, tmpa); \
        else if (value_is_string(tmpe)) \
            assert_vstr_eq(value_to_string(tmpe), tmpa); \
    } while (0)
#define assert_list_eq(expected, actual) do { \
        Value exp = expected, act = actual; \
        cr_assert(value_is_pair(act)); \
        assert_int_eq(length(exp), length(act)); \
        for (; exp != Qnil; exp = cdr(exp), act = cdr(act)) \
            assert_v_eq(car(exp), car(act)); \
    } while (0)
#define assert_pair_eq(expcar, expcdr, act) do { \
        Value a = act; \
        cr_assert(value_is_pair(a)); \
        assert_v_eq(V(expcar), car(a)); \
        assert_v_eq(V(expcdr), cdr(a)); \
    } while (0)

#define assert_int_eq(exp, act) cr_assert(eq(int, exp, act))
#define assert_str_eq(exp, act) cr_assert_str_eq(act, exp)
#define assert_runtime_error(pattern, v) do { \
        assert_int_eq(Qundef, v); \
        char *m = strstr(error_message(), pattern); \
        cr_assert_not_null(m, \
                           "expected \"%s\" includes \"%s\" but not", \
                           error_message(), pattern); \
    } while (0)

#define assert_no_error(v) \
    cr_assert_neq(v, Qundef, "got error with a message: '%s'", error_message())
#define assert_type(exp, act) assert_str_eq(exp, value_type_to_string(value_type_of(act)))
#define assert_vx_eq(x, y, z, exp, act) do { \
        Value a = act; \
        assert_no_error(a); \
        assert_type(#x, a); \
        assert_##y##_eq(exp, value_to_##z(a)); \
    } while (0)
#define assert_x_parsed(x, exp, act) assert_##x(exp, parse_expr_string(act))
#define assert_x_evaled(x, exp, act) assert_##x(exp, eval_string(act))
#define assert_x_evaled1(x, act) assert_##x(eval_string(act))

#define assert_vint_eq(exp, act) assert_vx_eq(integer, int, int, exp, act)
#define assert_vstr_eq(exp, act) assert_vx_eq(string, str, string, exp, act)
#define assert_vsym_eq(exp, act) assert_vx_eq(symbol, str, string, exp, act)
#define assert_vtrue(act) assert_int_eq(Qtrue, act)
#define assert_vfalse(act) assert_int_eq(Qfalse, act)

#define assert_vint_eq_parsed(exp, act) assert_x_parsed(vint_eq, exp, act)
#define assert_vstr_eq_parsed(exp, act) assert_x_parsed(vstr_eq, exp, act)
#define assert_vsym_eq_parsed(exp, act) assert_x_parsed(vsym_eq, exp, act)
#define assert_list_eq_parsed(exp, act) assert_x_parsed(list_eq, exp, act)
#define assert_pair_eq_parsed(ecar, ecdr, act) assert_pair_eq(ecar, ecdr, parse_expr_string(act))
#define assert_runtime_error_parsed(exp, act) assert_x_parsed(runtime_error, exp, act)

#define assert_int_eq_evaled(exp, act) assert_x_evaled(int_eq, exp, act)
#define assert_vint_eq_evaled(exp, act) assert_x_evaled(vint_eq, exp, act)
#define assert_list_eq_evaled(exp, act) assert_x_evaled(list_eq, exp, act)
#define assert_pair_eq_evaled(ecar, ecdr, act) assert_pair_eq(ecar, ecdr, eval_string(act))
#define assert_runtime_error_evaled(exp, act) assert_x_evaled(runtime_error, exp, act)
#define assert_vtrue_evaled(act) assert_x_evaled1(vtrue, act)
#define assert_vfalse_evaled(act) assert_x_evaled1(vfalse, act)

TestSuite(lisp, .init = reset_environment);

Test(lisp, nil) {
    cr_assert(value_is_nil(Qnil));
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
    assert_list_eq_parsed(list(V(1), V(2), Qundef), "(1 2)");
}

Test(lisp, parse_string) {
    assert_vstr_eq_parsed("abc", "\"abc\"");
    assert_vstr_eq_parsed("a\\b", "\"a\\\\b\"");
    assert_vstr_eq_parsed("a\"b", "\"a\\\"b\"");
}

Test(lisp, parse_string_list) {
    assert_list_eq_parsed(list(V("abc"), V("def"), Qundef),
                          "(\"abc\" \"def\")");
}

Test(lisp, cxr) {
    assert_vint_eq(42, caaaar(parse_expr_string("((((42))))")));
}

Test(lisp, parse_ident) {
    assert_vsym_eq_parsed("a", "a");
}

Test(lisp, parse_dot) {
    assert_pair_eq_parsed(1, 2, "(1 . 2)");
}

Test(lisp, parse_peculiar) {
    assert_vint_eq_parsed(42, "+42");

    cr_assert(value_is_symbol(parse_expr_string("+")));
}

Test(lisp, parse_lambda) {
    assert_list_eq_parsed(list(Vsym("lambda"), Qnil, V(42), Qundef),
                          "(lambda () 42)");
}

Test(lisp, parse_broken) {
    assert_runtime_error_parsed("got 'EOF'", "(");
    assert_runtime_error_parsed("got '''", "'");
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

Test(lisp, null) {
    assert_vtrue_evaled("(null? ())");
    assert_vtrue_evaled("(null? (list))");
    assert_vfalse_evaled("(null? (list 1))");
    assert_vfalse_evaled("(null? 1)");
}

Test(lisp, reverse) {
    assert_list_eq_evaled(Qnil, "()");
    assert_list_eq_evaled(list(V(1), Qundef), "(reverse (list 1))");
    assert_list_eq_evaled(list(V(2), V(1), Qundef), "(reverse (list 1 2))");
    assert_list_eq_evaled(list(V(3), V(2), V(1), Qundef), "(reverse (list 1 2 3))");
}

Test(lisp, cons_etc) {
    assert_pair_eq_evaled(1, 2, "(cons 1 2)");

    assert_vint_eq_evaled(1, "(car (cons 1 2))");
    assert_vint_eq_evaled(2, "(cdr (cons 1 2))");
}

Test(lisp, define_variable) {
    assert_vint_eq_evaled(42, "(define x 42) x");
    assert_vint_eq_evaled(-42, "(define x (* -1 42)) x");
}

Test(lisp, define_function) {
    assert_vint_eq_evaled(42, "(define (f) 42) (f)");
    assert_vint_eq_evaled(-42, "(define (f x) (* -1 x)) (f 42)");
}

Test(lisp, define_function_variadic) {
    assert_vint_eq_evaled(42, "(define (f . a) 42) (f)");
    assert_vint_eq_evaled(-42, "(define (f . a) (* -1 (car a))) (f 42)");
}

Test(lisp, set) {
    assert_runtime_error_evaled("unbound variable: x", "(set! x 42) x");
    assert_vint_eq_evaled(42, "(define x 1) (set! x 42) x");
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
"   (myeven? 88))");
}

Test(lisp, applicable) {
    assert_runtime_error_evaled("expected applicative", "(1 1)");
    assert_runtime_error_evaled("expected applicative", "(() 1)");
}

Test(lisp, apply_variadic) {
    assert_vtrue_evaled("(= 1 1 1 1 1 1 1 1 1 1)");
}

Test(lisp, lambda) {
    cr_assert(value_is_closure(eval_string("(lambda () 1)")));

    assert_vint_eq_evaled(42, "((lambda () 42))");
    assert_vint_eq_evaled(42, "((lambda (x) (* 2 x)) 21)");
    assert_vint_eq_evaled(42, "((lambda (x y) (* x y)) 3 14)");
    assert_vint_eq_evaled(42, "(define mul (lambda (x y) (* x y))) (mul 3 14)");
    assert_vint_eq_evaled(42, "(define a 42) ((lambda () a))");
    assert_vint_eq_evaled(42, "(define a 42) ((lambda () ((lambda () a))))");
    assert_vint_eq_evaled(42, "(define a 42) ((lambda (a) a) 10) a");
}

Test(lisp, lambda2) {
    assert_vint_eq_evaled(42,
    "(define a 42)"
    "(define f (lambda () a))"
    "(define g (lambda () f))"
    "((g))");
    assert_vint_eq_evaled(42,
    "(define a 42)"
    "(define f (lambda () a))"
    "(((lambda () f)))");
    assert_vint_eq_evaled(42,
    "(define a 42)"
    "(define f (lambda () (lambda () a)))"
    "(define g (f))"
    "(g)");
    assert_vint_eq_evaled(42,
    "(define a 42)"
    "(((lambda () (lambda () a))))");
    assert_vint_eq_evaled(42,
    "(define a 42)"
    "(define f (lambda () a))"
    "((((lambda () (lambda () f)))))");
    assert_vint_eq_evaled(42,
    "(((lambda () (lambda () 42))))");
    assert_vint_eq_evaled(42,
    "((((lambda () (lambda () (lambda () 42))))))");
}

Test(lisp, let_is_lambda) {
    assert_vint_eq_evaled(42, "((lambda (x) x) 42)");
    assert_vint_eq_evaled(63, "((lambda (x y) (+ x y)) 42 21)");
    assert_vint_eq_evaled(63, "((lambda (x) ((lambda (y) (+ x y)) 21)) 42)");
    assert_vint_eq_evaled(1, "((lambda (x) ((lambda (x) x) 1)) 42)");
    assert_vint_eq_evaled(42, "((lambda (x) ((lambda (y) y) x)) 42)");
    assert_vint_eq_evaled(42, "((lambda (x) ((lambda (x) x) x)) 42)");
    assert_vint_eq_evaled(42, "((lambda (x) ((lambda (x) x) 10) x) 42)");
    assert_list_eq_evaled(list(V(42), V(10), Qundef),
                          "((lambda (x) ((lambda (y) (list x y)) 10)) 42)");
}

Test(lisp, lambda_rec) {
    assert_vint_eq_evaled(1, "(define f (lambda (x) (if (> x 0) x (f (+ x 1))))) (f 0)");
}

Test(lisp, lambda_variadic) {
    cr_assert(value_is_closure(eval_string("(lambda x 1)")));

    assert_vint_eq_evaled(42, "((lambda x 42))");
    assert_vint_eq_evaled(42, "((lambda x (* 2 (car x))) 21)");
    assert_vint_eq_evaled(42, "((lambda x (* (car x) (car (cdr x)))) 3 14)");
    assert_vint_eq_evaled(42, "(define mul (lambda x (* (car x) (car (cdr x))))) (mul 3 14)");
    assert_vint_eq_evaled(42, "(define a 42) ((lambda x a))");
    assert_vint_eq_evaled(42, "(define a 42) ((lambda x ((lambda x a))))");
    assert_vint_eq_evaled(42, "(define a 42) ((lambda a (car a)) 10) a");
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

Test(lisp, callcc) {
    // From Kawa's test suite under the MIT license:
    // https://gitlab.com/kashell/Kawa/-/blob/master/testsuite/r5rs_pitfall.scm
    assert_list_eq_evaled(list(V(5), V(4), V(3), V(2), V(1), V(0), Qundef),
"(let ((x ())"
"      (y 0))"
"  (call/cc "
"   (lambda (escape)"
"     (let* ((in ((lambda (foo) "
"                   (set! x (cons y x))"
"                   (if (= y 5)"
"                       (escape x)"
"                       (begin"
"                         (set! y 0)"
"                         foo)))"
"                 (call/cc (lambda (bar) bar))))"
"            (yo ((lambda (foo) "
"                   (set! y (+ y 1))"
"                   foo)"
"                 (call/cc (lambda (baz) baz)))))"
"       (in yo)))))");
}

Test(lisp, eq) {
    assert_vtrue_evaled("(eq? #t #t)");
    assert_vtrue_evaled("(eq? #f #f)");
    assert_vfalse_evaled("(eq? #t #f)");
    assert_vtrue_evaled("(eq? 1 1)");
    assert_vfalse_evaled("(eq? 1 -1)");
    assert_vtrue_evaled("(eq? () ())");
    assert_vfalse_evaled("(eq? () (list 1))");
    assert_vtrue_evaled("(let ((x (list 1))) (eq? x x))");
    assert_vtrue_evaled("(let ((p (lambda (x) x))) (eq? p p))");

    assert_vfalse_evaled("(eq? (list 1) (list 1))");
    assert_vfalse_evaled("(eq? (list 1 (list 2)) (list 1 (list 2)))");
}

Test(lisp, equal) {
    assert_vtrue_evaled("(equal? #t #t)");
    assert_vtrue_evaled("(equal? #f #f)");
    assert_vfalse_evaled("(equal? #t #f)");
    assert_vtrue_evaled("(equal? 1 1)");
    assert_vfalse_evaled("(equal? 1 -1)");
    assert_vtrue_evaled("(equal? () ())");
    assert_vfalse_evaled("(equal? () (list 1))");
    assert_vtrue_evaled("(let ((x (list 1))) (equal? x x))");
    assert_vtrue_evaled("(let ((p (lambda (x) x))) (equal? p p))");

    assert_vtrue_evaled("(equal? (list 1) (list 1))");
    assert_vtrue_evaled("(equal? (list 1 (list 2)) (list 1 (list 2)))");

    assert_vtrue_evaled("(equal? \"abc\" \"abc\")");
    assert_vtrue_evaled("(equal? \"\" \"\")");
    assert_vfalse_evaled("(equal? \"abc\" \"abd\")");
    assert_vfalse_evaled("(equal? \"abc\" \"\")");
}
