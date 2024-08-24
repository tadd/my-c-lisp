#include <stdlib.h>
#include <string.h>

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

#define expect_vint_eq(exp, act) expect_vx_eq(integer, int, int, exp, act)
#define expect_vstr_eq(exp, act) expect_vx_eq(string, str, string, exp, act)
#define expect_vsym_eq(exp, act) expect_vx_eq(symbol, str, string, exp, act)

#define expect_vint_eq_parsed(exp, act) expect_x_parsed(vint_eq, exp, act)
#define expect_vstr_eq_parsed(exp, act) expect_x_parsed(vstr_eq, exp, act)
#define expect_vsym_eq_parsed(exp, act) expect_x_parsed(vsym_eq, exp, act)
#define expect_list_eq_parsed(exp, act) expect_x_parsed(list_eq, exp, act)
#define expect_pair_eq_parsed(ecar, ecdr, act) expect_pair_eq(ecar, ecdr, parse_expr_string(act))
#define expect_runtime_error_parsed(exp, act) expect_x_parsed(runtime_error, exp, act)

#define expect_runtime_error_evaled(exp, act) expect_x_evaled(runtime_error, exp, act)

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

Test(lisp, div0) {
    expect_runtime_error_evaled("divided by zero", "(/ 42 0)");
}

Test(lisp, modulo) {
    expect_runtime_error_evaled("divided by zero", "(modulo 13 0)");
}

Test(lisp, unbound_variable) {
    expect_runtime_error_evaled("unbound variable: x", "x");
    expect_runtime_error_evaled("unbound variable: x", "(+ x 2)");
}

Test(lisp, if) {
    expect_runtime_error_evaled("2..3 but got 1", "(if #f)");
    expect_runtime_error_evaled("2..3 but got 4", "(if #f 1 2 3)");
}

Test(lisp, set) {
    expect_runtime_error_evaled("unbound variable: x", "(begin (set! x 42) x)");
}

Test(lisp, let) {
    expect_runtime_error_evaled("one or more expressions",
                                "(let ((x 42)))");
    expect_runtime_error_evaled("one or more expressions",
                                "(let ((x 42) (y 100)))");
}

Test(lisp, applicable) {
    expect_runtime_error_evaled("expected applicative", "(1 1)");
    expect_runtime_error_evaled("expected applicative", "(() 1)");
}