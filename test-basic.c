#include <stdio.h>
#include <stdlib.h>

#include <criterion/criterion.h>
#include <criterion/new/assert.h>

#include "lisp.h"

#define expect_stringify(exp, v) do { \
        char *s = stringify(v); \
        cr_expect_str_eq(s, exp);   \
        free(s); \
    } while (0)

#define value_idfunc list
#define V(x) \
    _Generic(x, int: value_of_int, char *: value_of_string, Value: value_idfunc)(x)
#define Vsym(x) value_of_symbol(x)
#define expect_v_eq(exp, act)  do { \
        Value tmpe = exp, tmpa = act; \
        if (value_is_int(tmpe)) \
            expect_int_eq(tmpe, tmpa); \
        else if (value_is_string(tmpe)) \
            expect_vstr_eq(value_to_string(tmpe), tmpa); \
    } while (0)
#define expect_list_eq(expected, actual) do { \
        Value exp = expected, act = actual; \
        cr_expect(value_is_pair(act)); \
        expect_int_eq(length(exp), length(act)); \
        for (; exp != Qnil; exp = cdr(exp), act = cdr(act)) \
            expect_v_eq(car(exp), car(act)); \
    } while (0)
#define expect_pair_eq(expcar, expcdr, act) do { \
        Value a = act; \
        cr_expect(value_is_pair(a)); \
        expect_v_eq(V(expcar), car(a)); \
        expect_v_eq(V(expcdr), cdr(a)); \
    } while (0)

#define expect_int_eq(exp, act) cr_expect(eq(int, exp, act))
#define expect_str_eq(exp, act) cr_expect_str_eq(act, exp)
#define expect_runtime_error(pattern, v) do { \
        expect_int_eq(Qundef, v); \
        char *m = strstr(error_message(), pattern); \
        cr_expect_not_null(m, \
                           "expected \"%s\" includes \"%s\" but not", \
                           error_message(), pattern); \
    } while (0)

#define expect_no_error(v) \
    cr_expect_neq(v, Qundef, "got error with a message: '%s'", error_message())
#define expect_type(exp, act) expect_str_eq(exp, value_type_to_string(value_type_of(act)))
#define expect_vx_eq(x, y, z, exp, act) do { \
        Value a = act; \
        expect_no_error(a); \
        expect_type(#x, a); \
        expect_##y##_eq(exp, value_to_##z(a)); \
    } while (0)
#define expect_x_parsed(x, exp, act) expect_##x(exp, parse_expr_string(act))
#define expect_x_evaled(x, exp, act) expect_##x(exp, eval_string(act))
#define expect_x_evaled1(x, act) expect_##x(eval_string(act))

#define expect_vint_eq(exp, act) expect_vx_eq(integer, int, int, exp, act)
#define expect_vstr_eq(exp, act) expect_vx_eq(string, str, string, exp, act)
#define expect_vsym_eq(exp, act) expect_vx_eq(symbol, str, string, exp, act)
#define expect_vtrue(act) expect_int_eq(Qtrue, act)
#define expect_vfalse(act) expect_int_eq(Qfalse, act)

#define expect_vint_eq_parsed(exp, act) expect_x_parsed(vint_eq, exp, act)
#define expect_vstr_eq_parsed(exp, act) expect_x_parsed(vstr_eq, exp, act)
#define expect_vsym_eq_parsed(exp, act) expect_x_parsed(vsym_eq, exp, act)
#define expect_list_eq_parsed(exp, act) expect_x_parsed(list_eq, exp, act)
#define expect_pair_eq_parsed(ecar, ecdr, act) expect_pair_eq(ecar, ecdr, parse_expr_string(act))
#define expect_runtime_error_parsed(exp, act) expect_x_parsed(runtime_error, exp, act)

#define expect_int_eq_evaled(exp, act) expect_x_evaled(int_eq, exp, act)
#define expect_vint_eq_evaled(exp, act) expect_x_evaled(vint_eq, exp, act)
#define expect_list_eq_evaled(exp, act) expect_x_evaled(list_eq, exp, act)
#define expect_pair_eq_evaled(ecar, ecdr, act) expect_pair_eq(ecar, ecdr, eval_string(act))
#define expect_runtime_error_evaled(exp, act) expect_x_evaled(runtime_error, exp, act)
#define expect_vtrue_evaled(act) expect_x_evaled1(vtrue, act)
#define expect_vfalse_evaled(act) expect_x_evaled1(vfalse, act)

TestSuite(lisp, .init = reset_environment);

Test(lisp, nil) {
    cr_expect(value_is_nil(Qnil));
}

Test(lisp, printing) {
    expect_stringify("#t", Qtrue);
    expect_stringify("#f", Qfalse);
    expect_stringify("<undef>", Qundef);
    expect_stringify("()", Qnil);

    expect_stringify("0", value_of_int(0));
    expect_stringify("42", value_of_int(42));
    expect_stringify("-42", value_of_int(-42));

    expect_stringify("'foo", value_of_symbol("foo"));

    expect_stringify("(1)", cons(V(1), Qnil));
    expect_stringify("(1 . 2)", cons(V(1), V(2)));
    expect_stringify("(1 2)", list(V(1), V(2), Qundef));
}

Test(lisp, parse_int) {
    expect_vint_eq_parsed(42, "42");
    expect_vint_eq_parsed(-42, "-42");
}

Test(lisp, parse_nil) {
    cr_expect(value_is_nil(parse_expr_string("()")));
}

Test(lisp, parse_list) {
    expect_list_eq_parsed(list(V(1), V(2), Qundef), "(1 2)");
}

Test(lisp, parse_string) {
    expect_vstr_eq_parsed("abc", "\"abc\"");
    expect_vstr_eq_parsed("a\\b", "\"a\\\\b\"");
    expect_vstr_eq_parsed("a\"b", "\"a\\\"b\"");
}

Test(lisp, parse_string_list) {
    expect_list_eq_parsed(list(V("abc"), V("def"), Qundef),
                          "(\"abc\" \"def\")");
}

Test(lisp, cxr) {
    expect_vint_eq(42, caaaar(parse_expr_string("((((42))))")));
}

Test(lisp, parse_ident) {
    expect_vsym_eq_parsed("a", "a");
}

Test(lisp, parse_dot) {
    expect_pair_eq_parsed(1, 2, "(1 . 2)");
}

Test(lisp, parse_peculiar) {
    expect_vint_eq_parsed(42, "+42");

    cr_expect(value_is_symbol(parse_expr_string("+")));
}

Test(lisp, parse_lambda) {
    expect_list_eq_parsed(list(Vsym("lambda"), Qnil, V(42), Qundef),
                          "(lambda () 42)");
}

Test(lisp, parse_broken) {
    expect_runtime_error_parsed("got 'EOF'", "(");
    expect_runtime_error_parsed("got '''", "'");
}

Test(lisp, eval_arithmetic_literal) {
    expect_vint_eq_evaled(63, "(+ 42 21)");
    expect_vint_eq_evaled(21, "(- 42 21)");
    expect_vint_eq_evaled(8, "(* 4 2)");
    expect_vint_eq_evaled(2, "(/ 4 2)");
}

Test(lisp, eval_arithmetic_expr) {
    expect_vint_eq_evaled(63, "(+ (+ 40 2) 21)");
    expect_vint_eq_evaled(42, "(+ (- 40 4) (* 3 (/ 100 50)))");
}

Test(lisp, div0) {
    expect_runtime_error_evaled("divided by zero", "(/ 42 0)");
}

Test(lisp, relop) {
    expect_vtrue_evaled("(= 42 42)");
    expect_vtrue_evaled("(= 0 0 0 0 0)");

    expect_vfalse_evaled("(= 42 0)");
    expect_vfalse_evaled("(= 0 0 0 0 42)");

    expect_vtrue_evaled("(< 2 4)");
    expect_vtrue_evaled("(< 2 3 4 5)");

    expect_vfalse_evaled("(< 2 0)");
    expect_vfalse_evaled("(< 2 3 4 4)");

    expect_vtrue_evaled("(<= 2 4)");
    expect_vtrue_evaled("(<= 2 3 4 4)");

    expect_vfalse_evaled("(<= 2 0)");
    expect_vfalse_evaled("(<= 2 3 4 3)");

    expect_vtrue_evaled("(> 3 2)");
    expect_vtrue_evaled("(> 4 3 2 1)");

    expect_vfalse_evaled("(> 0 1)");
    expect_vfalse_evaled("(> 4 3 2 2)");

    expect_vtrue_evaled("(>= 3 2)");
    expect_vtrue_evaled("(>= 4 3 2 2)");

    expect_vfalse_evaled("(>= 0 1)");
    expect_vfalse_evaled("(>= 4 3 2 3)");
}

Test(lisp, modulo) {
    expect_vint_eq_evaled(1, "(modulo 13 4)");
    expect_vint_eq_evaled(3, "(modulo -13 4)");
    expect_vint_eq_evaled(-3, "(modulo 13 -4)");
    expect_vint_eq_evaled(-1, "(modulo -13 -4)");

    expect_runtime_error_evaled("divided by zero", "(modulo 13 0)");
}

Test(lisp, unbound_variable) {
    expect_runtime_error_evaled("unbound variable: x", "x");
    expect_runtime_error_evaled("unbound variable: x", "(+ x 2)");
}

Test(lisp, true_false) {
    expect_int_eq_evaled(Qtrue, "#t");
    expect_int_eq_evaled(Qfalse, "#f");
}

Test(lisp, if) {
    expect_vint_eq_evaled(1, "(if #t 1)");
    expect_vint_eq_evaled(1, "(if #t 1 2)");
    expect_vint_eq_evaled(2, "(if #f 1 2)");

    expect_runtime_error_evaled("2..3 but got 1", "(if #f)");
    expect_runtime_error_evaled("2..3 but got 4", "(if #f 1 2 3)");
}

Test(lisp, if_composed) {
    expect_vint_eq_evaled(3, "(if (if #t 1 #f) (if #t 3 4) (if #t 5 6))");
    expect_vint_eq_evaled(6, "(if (if #f 1 #f) (if #f 3 4) (if #f 5 6))");
}

Test(lisp, list) {
    expect_int_eq_evaled(Qnil, "()");
    expect_int_eq_evaled(Qnil, "(list)");

    expect_list_eq_evaled(list(V(42), Qundef), "(list 42)");

    Value v = list(V(42), V("foo"), Qundef);
    cr_expect(value_is_pair(v));
    expect_int_eq(2, length(v));
    expect_vint_eq(42, car(v));
    expect_vstr_eq("foo", cadr(v));
}

Test(lisp, null) {
    expect_vtrue_evaled("(null? ())");
    expect_vtrue_evaled("(null? (list))");
    expect_vfalse_evaled("(null? (list 1))");
    expect_vfalse_evaled("(null? 1)");
}

Test(lisp, reverse) {
    expect_list_eq_evaled(Qnil, "()");
    expect_list_eq_evaled(list(V(1), Qundef), "(reverse (list 1))");
    expect_list_eq_evaled(list(V(2), V(1), Qundef), "(reverse (list 1 2))");
    expect_list_eq_evaled(list(V(3), V(2), V(1), Qundef), "(reverse (list 1 2 3))");
}

Test(lisp, cons_etc) {
    expect_pair_eq_evaled(1, 2, "(cons 1 2)");

    expect_vint_eq_evaled(1, "(car (cons 1 2))");
    expect_vint_eq_evaled(2, "(cdr (cons 1 2))");
}

Test(lisp, define_variable) {
    expect_vint_eq_evaled(42, "(define x 42) x");
    expect_vint_eq_evaled(-42, "(define x (* -1 42)) x");
}

Test(lisp, define_function) {
    expect_vint_eq_evaled(42, "(define (f) 42) (f)");
    expect_vint_eq_evaled(-42, "(define (f x) (* -1 x)) (f 42)");
}

Test(lisp, define_function_variadic) {
    expect_vint_eq_evaled(42, "(define (f . a) 42) (f)");
    expect_vint_eq_evaled(-42, "(define (f . a) (* -1 (car a))) (f 42)");
}

Test(lisp, set) {
    expect_runtime_error_evaled("unbound variable: x", "(set! x 42) x");
    expect_vint_eq_evaled(42, "(define x 1) (set! x 42) x");
}

Test(lisp, let) {
    expect_vint_eq_evaled(42, "(let ((x 42)) x)");
    expect_vint_eq_evaled(63, "(let ((x 42) (y 21)) (+ x y))");
    expect_vint_eq_evaled(63, "(let ((x 42)) (let ((y 21)) (+ x y)))");
    expect_vint_eq_evaled(1, "(let ((x 42)) (let ((x 1)) x))");
    expect_vint_eq_evaled(42, "(let ((x 42)) (let ((y x)) y))");
    expect_vint_eq_evaled(42, "(let ((x 42)) (let ((x x)) x))");
    expect_vint_eq_evaled(42, "(let ((x 42)) (let ((x 10)) x) x)");
    expect_list_eq_evaled(list(V(42), V(10), Qundef), "(let ((x 42) (y 10)) (list x y))");

    expect_runtime_error_evaled("one or more expressions", "(let ((x 42)))");
    expect_runtime_error_evaled("one or more expressions", "(let ((x 42) (y 100)))");
}

Test(lisp, let_body_define) {
    expect_vint_eq_evaled(2, "(let ((x 42)) (define x 2) x)");
    expect_vint_eq_evaled(2, "(define x 1) (let ((x 42)) (define x 2) x)");
    expect_vint_eq_evaled(1, "(define x 1) (let () (define x 2) x) x");
}

Test(lisp, let_star) {
    expect_list_eq_evaled(list(V(42), V(10), Qundef),
                          "(let* ((x 42) (y 10)) (list x y))");
}

Test(lisp, letrec) {
    expect_vtrue_evaled(
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
    expect_runtime_error_evaled("expected applicative", "(1 1)");
    expect_runtime_error_evaled("expected applicative", "(() 1)");
}

Test(lisp, apply_variadic) {
    expect_vtrue_evaled("(= 1 1 1 1 1 1 1 1 1 1)");
}

Test(lisp, lambda) {
    cr_expect(value_is_closure(eval_string("(lambda () 1)")));

    expect_vint_eq_evaled(42, "((lambda () 42))");
    expect_vint_eq_evaled(42, "((lambda (x) (* 2 x)) 21)");
    expect_vint_eq_evaled(42, "((lambda (x y) (* x y)) 3 14)");
    expect_vint_eq_evaled(42, "(define mul (lambda (x y) (* x y))) (mul 3 14)");
    expect_vint_eq_evaled(42, "(define a 42) ((lambda () a))");
    expect_vint_eq_evaled(42, "(define a 42) ((lambda () ((lambda () a))))");
    expect_vint_eq_evaled(42, "(define a 42) ((lambda (a) a) 10) a");
}

Test(lisp, lambda2) {
    expect_vint_eq_evaled(42,
    "(define a 42)"
    "(define f (lambda () a))"
    "(define g (lambda () f))"
    "((g))");
    expect_vint_eq_evaled(42,
    "(define a 42)"
    "(define f (lambda () a))"
    "(((lambda () f)))");
    expect_vint_eq_evaled(42,
    "(define a 42)"
    "(define f (lambda () (lambda () a)))"
    "(define g (f))"
    "(g)");
    expect_vint_eq_evaled(42,
    "(define a 42)"
    "(((lambda () (lambda () a))))");
    expect_vint_eq_evaled(42,
    "(define a 42)"
    "(define f (lambda () a))"
    "((((lambda () (lambda () f)))))");
    expect_vint_eq_evaled(42,
    "(((lambda () (lambda () 42))))");
    expect_vint_eq_evaled(42,
    "((((lambda () (lambda () (lambda () 42))))))");
}

Test(lisp, let_is_lambda) {
    expect_vint_eq_evaled(42, "((lambda (x) x) 42)");
    expect_vint_eq_evaled(63, "((lambda (x y) (+ x y)) 42 21)");
    expect_vint_eq_evaled(63, "((lambda (x) ((lambda (y) (+ x y)) 21)) 42)");
    expect_vint_eq_evaled(1, "((lambda (x) ((lambda (x) x) 1)) 42)");
    expect_vint_eq_evaled(42, "((lambda (x) ((lambda (y) y) x)) 42)");
    expect_vint_eq_evaled(42, "((lambda (x) ((lambda (x) x) x)) 42)");
    expect_vint_eq_evaled(42, "((lambda (x) ((lambda (x) x) 10) x) 42)");
    expect_list_eq_evaled(list(V(42), V(10), Qundef),
                          "((lambda (x) ((lambda (y) (list x y)) 10)) 42)");
}

Test(lisp, lambda_rec) {
    expect_vint_eq_evaled(1, "(define f (lambda (x) (if (> x 0) x (f (+ x 1))))) (f 0)");
}

Test(lisp, lambda_variadic) {
    cr_expect(value_is_closure(eval_string("(lambda x 1)")));

    expect_vint_eq_evaled(42, "((lambda x 42))");
    expect_vint_eq_evaled(42, "((lambda x (* 2 (car x))) 21)");
    expect_vint_eq_evaled(42, "((lambda x (* (car x) (car (cdr x)))) 3 14)");
    expect_vint_eq_evaled(42, "(define mul (lambda x (* (car x) (car (cdr x))))) (mul 3 14)");
    expect_vint_eq_evaled(42, "(define a 42) ((lambda x a))");
    expect_vint_eq_evaled(42, "(define a 42) ((lambda x ((lambda x a))))");
    expect_vint_eq_evaled(42, "(define a 42) ((lambda a (car a)) 10) a");
}

Test(lisp, begin) {
    expect_vint_eq_evaled(3, "(begin 1 2 3)");
}

Test(lisp, cond) {
    expect_vint_eq_evaled(2, "(cond (#f 1) (#t 2) (else 3))");
    expect_vint_eq_evaled(3, "(cond (#f 1) (else 3))");
    expect_vint_eq_evaled(2, "(cond (#f) (2))");
}

Test(lisp, cputime) {
    Value v = eval_string("(_cputime)");
    cr_expect(value_is_int(v));
    cr_expect(gt(int, value_to_int(v), 0));
}

Test(lisp, comment) {
    expect_vint_eq_evaled(1, "1 ; foo");
    expect_vint_eq_evaled(2, "2 ;;bar");
    expect_list_eq_evaled(list(V(1), V(2), Qundef), "(list 1 ;;; ?? ;;; \n 2)");
}

Test(lisp, define_and_lambda) {
    expect_vint_eq_evaled(42,
"(define f (lambda () (g)))"
"(define g (lambda () 42))"
"(f)");
}

Test(lisp, callcc) {
    // Based on "yin-yang" in Kawa's test suite under the MIT license:
    // https://gitlab.com/kashell/Kawa/-/blob/master/testsuite/r5rs_pitfall.scm
    expect_list_eq_evaled(list(V(5), V(4), V(3), V(2), V(1), V(0), Qundef),
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
    expect_vtrue_evaled("(eq? #t #t)");
    expect_vtrue_evaled("(eq? #f #f)");
    expect_vfalse_evaled("(eq? #t #f)");
    expect_vtrue_evaled("(eq? 1 1)");
    expect_vfalse_evaled("(eq? 1 -1)");
    expect_vtrue_evaled("(eq? () ())");
    expect_vfalse_evaled("(eq? () (list 1))");
    expect_vtrue_evaled("(let ((x (list 1))) (eq? x x))");
    expect_vtrue_evaled("(let ((p (lambda (x) x))) (eq? p p))");

    expect_vfalse_evaled("(eq? (list 1) (list 1))");
    expect_vfalse_evaled("(eq? (list 1 (list 2)) (list 1 (list 2)))");
}

Test(lisp, equal) {
    expect_vtrue_evaled("(equal? #t #t)");
    expect_vtrue_evaled("(equal? #f #f)");
    expect_vfalse_evaled("(equal? #t #f)");
    expect_vtrue_evaled("(equal? 1 1)");
    expect_vfalse_evaled("(equal? 1 -1)");
    expect_vtrue_evaled("(equal? () ())");
    expect_vfalse_evaled("(equal? () (list 1))");
    expect_vtrue_evaled("(let ((x (list 1))) (equal? x x))");
    expect_vtrue_evaled("(let ((p (lambda (x) x))) (equal? p p))");

    expect_vtrue_evaled("(equal? (list 1) (list 1))");
    expect_vtrue_evaled("(equal? (list 1 (list 2)) (list 1 (list 2)))");

    expect_vtrue_evaled("(equal? \"abc\" \"abc\")");
    expect_vtrue_evaled("(equal? \"\" \"\")");
    expect_vfalse_evaled("(equal? \"abc\" \"abd\")");
    expect_vfalse_evaled("(equal? \"abc\" \"\")");
}

Test(lisp, append) {
    expect_list_eq_evaled(Qnil, "(append)");
    expect_list_eq_evaled(list(V(1), Qundef),
                          "(append (list 1))");
    expect_list_eq_evaled(list(V(1), V(2), Qundef),
                          "(append (list 1) (list 2))");
    expect_list_eq_evaled(list(V(1), V(2), V(3), Qundef),
                          "(append (list 1) (list 2 3))");
    expect_list_eq_evaled(list(V(1), list(V(2), Qundef), list(V(3), Qundef), Qundef),
                          "(append (list 1 (list 2)) (list (list 3)))");
    Value v = eval_string("(append (list 1 2) (cons 3 4))");
    // expects: '(1 2 3 . 4)
    expect_vint_eq(1, car(v));
    expect_vint_eq(2, cadr(v));
    expect_vint_eq(3, caddr(v));
    expect_vint_eq(4, cdddr(v));
    expect_list_eq_evaled(list(V(1), V(2), V(3), Qundef),
                          "(append (list 1) (list 2) (list 3))");
}

Test(lisp, apply) {
    expect_vint_eq_evaled(42, "(apply + (list 42))");
    expect_vint_eq_evaled(43, "(apply + 1 (list 42))");
    expect_vint_eq_evaled(45, "(apply + 1 2 (list 42))");
    expect_vint_eq_evaled(48, "(apply + 1 2 3 (list 42))");
    expect_vint_eq_evaled(48, "(apply + (list 1 2 3 42))");
    expect_vint_eq_evaled(48, "(apply + (apply + (list 1 2 3)) (list 42))");
    expect_vtrue_evaled("(apply equal? (list (list 1) (list 1)))");
}
