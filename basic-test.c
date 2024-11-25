#include <stdlib.h>
#include <string.h>

#include <criterion/criterion.h>
#include <criterion/new/assert.h>

#include "schaf.h"

#define expect_stringify(exp, v) do { \
        char *s = stringify(v); \
        cr_expect_str_eq(s, exp); \
        free(s); \
    } while (0)

#define value_idfunc list
#define V(x) \
    _Generic(x, int: value_of_int, char *: value_of_string, Value: value_idfunc)(x)
#define expect_v_eq(expected, actual) do { \
        Value vexp = expected, vact = actual; \
        if (value_is_int(vexp)) \
            expect_int_eq(vexp, vact); \
        else if (value_is_string(vexp)) \
            expect_vstr_eq(value_to_string(vexp), vact); \
        else if (value_is_symbol(vexp)) \
            expect_vsym_eq(value_to_string(vexp), vact); \
    } while (0)
#define expect_list_eq(expected, actual) do { \
        Value exp = expected, act = actual; \
        cr_expect(value_is_pair(act)); \
        expect_int_eq(length(exp), length(act)); \
        for (; exp != Qnil; exp = cdr(exp), act = cdr(act)) \
            expect_v_eq(car(exp), car(act)); \
    } while (0)
#define expect_pair_eq(ecar, ecdr, act) do { \
        Value a = act; \
        cr_expect(value_is_pair(a)); \
        expect_v_eq(V(ecar), car(a)); \
        expect_v_eq(V(ecdr), cdr(a)); \
    } while (0)

#define expect_int_eq(exp, act) cr_expect(eq(int, exp, act))
#define expect_string_eq(exp, act) cr_expect_str_eq(act, exp)
#define expect_error(pattern, v) do { \
        expect_int_eq(Qundef, v); \
        char *m = strstr(error_message(), pattern); \
        cr_expect_not_null(m, "expected \"%s\" includes \"%s\"", \
                           error_message(), pattern); \
    } while (0)

#define expect_no_error(v) \
    cr_expect_neq(v, Qundef, "got error with a message: '%s'", error_message())
#define expect_vx_eq(t, n, exp, act) do { \
        Value a = act; \
        expect_no_error(a); \
        expect_int_eq(t, value_type_of(a)); \
        expect_##n##_eq(exp, value_to_##n(a)); \
    } while (0)
#define expect_x_eq_parsed(x, exp, act) expect_##x##_eq(exp, parse_expr_string(act))

#define expect_vint_eq(exp, act) expect_vx_eq(TYPE_INT, int, exp, act)
#define expect_vstr_eq(exp, act) expect_vx_eq(TYPE_STR, string, exp, act)
#define expect_vsym_eq(exp, act) expect_vx_eq(TYPE_SYMBOL, string, exp, act)

#define expect_vint_eq_parsed(exp, act) expect_x_eq_parsed(vint, exp, act)
#define expect_vstr_eq_parsed(exp, act) expect_x_eq_parsed(vstr, exp, act)
#define expect_vsym_eq_parsed(exp, act) expect_x_eq_parsed(vsym, exp, act)
#define expect_list_eq_parsed(exp, act) expect_x_eq_parsed(list, exp, act)
#define expect_pair_eq_parsed(ecar, ecdr, act) expect_pair_eq(ecar, ecdr, parse_expr_string(act))
#define expect_parse_error(exp, act) expect_error(exp, parse_expr_string(act))
#define expect_runtime_error(exp, act) expect_error(exp, eval_string(act))

static Value parse_expr_string(const char *in)
{
    Value v = parse_string(in);
    if (v == Qundef || v == Qnil)
        return v;
    return car(v);
}

// terminate with Qundef
static Value list(Value arg, ...)
{
    if (arg == Qundef)
        return Qnil;
    Value vec[0x100] = { arg, }, o;
    va_list ap;
    va_start(ap, arg);
    long i = 1;
    while ((o = va_arg(ap, Value)) != Qundef)
        vec[i++] = o;
    va_end(ap);
    Value l = Qnil;
    while (--i >= 0)
        l = cons(vec[i], l);
    return l;
}

TestSuite(schaf, .init = sch_initialize);

Test(schaf, printing) {
    expect_stringify("#t", Qtrue);
    expect_stringify("#f", Qfalse);
    expect_stringify("<undef>", Qundef);
    expect_stringify("()", Qnil);

    expect_stringify("0", value_of_int(0));
    expect_stringify("42", value_of_int(42));
    expect_stringify("-42", value_of_int(-42));

    expect_stringify("foo", value_of_symbol("foo"));

    expect_stringify("(1)", cons(V(1), Qnil));
    expect_stringify("(1 . 2)", cons(V(1), V(2)));
    expect_stringify("(1 2)", list(V(1), V(2), Qundef));
}

Test(schaf, parse_int) {
    expect_vint_eq_parsed(42, "42");
    expect_vint_eq_parsed(-42, "-42");
}

Test(schaf, parse_nil) {
    cr_expect(parse_expr_string("()") == Qnil);
}

Test(schaf, parse_list) {
    expect_list_eq_parsed(list(V(1), V(2), Qundef), "(1 2)");
}

Test(schaf, parse_string) {
    expect_vstr_eq_parsed("abc", "\"abc\"");
    expect_vstr_eq_parsed("a\\b", "\"a\\\\b\"");
    expect_vstr_eq_parsed("a\"b", "\"a\\\"b\"");
}

Test(schaf, parse_string_list) {
    expect_list_eq_parsed(list(V("abc"), V("def"), Qundef),
                          "(\"abc\" \"def\")");
}

#define caaaar(x) (car(car(car(car(x)))))
Test(schaf, cxr) {
    expect_vint_eq(42, caaaar(parse_expr_string("((((42))))")));
}

Test(schaf, parse_ident) {
    expect_vsym_eq_parsed("a", "a");
}

Test(schaf, parse_dot) {
    expect_pair_eq_parsed(1, 2, "(1 . 2)");
}

Test(schaf, parse_peculiar) {
    expect_vint_eq_parsed(42, "+42");
    cr_expect(value_is_symbol(parse_expr_string("+")));
}

Test(schaf, parse_lambda) {
    expect_list_eq_parsed(list(value_of_symbol("lambda"), Qnil, V(42), Qundef),
                          "(lambda () 42)");
}

Test(schaf, parse_broken) {
    expect_parse_error("got 'EOF'", "(");
    expect_parse_error("got 'EOF'", "'");
}

Test(schaf, parse_error_line_column) {
    expect_parse_error("<inline>:1:3: ", "(1");
    expect_parse_error("<inline>:2:3: ", "\n(1");
    expect_parse_error("<inline>:2:5: ", "()\n()(1");
}

Test(schaf, runtime_error_line_column) {
    expect_runtime_error(
"unbound variable: h\n"
"\t<inline>:2:14 in 'g'\n"
"\t<inline>:1:14 in 'f'\n"
"\t<inline>:3:2 in <toplevel>"
,
"(define (f) (g))\n"
"(define (g) (h))\n"
"(f)");
}

Test(schaf, div0) {
    expect_runtime_error("divided by zero", "(/ 42 0)");
}

Test(schaf, modulo) {
    expect_runtime_error("divided by zero", "(modulo 13 0)");
}

Test(schaf, unbound_variable) {
    expect_runtime_error("unbound variable: x", "x");
    expect_runtime_error("unbound variable: x", "(+ x 2)");
}

Test(schaf, if) {
    expect_runtime_error("2..3 but got 1", "(if #f)");
    expect_runtime_error("2..3 but got 4", "(if #f 1 2 3)");
}

Test(schaf, set) {
    expect_runtime_error("unbound variable: x", "(begin (set! x 42) x)");
}

Test(schaf, let) {
    expect_runtime_error("but got 1", "(let ((x 42)))");
    expect_runtime_error("but got 1", "(let ((x 42) (y 100)))");
}

Test(schaf, applicable) {
    expect_runtime_error("expected procedure", "(1 1)");
    expect_runtime_error("expected procedure", "(() 1)");
}

Test(schaf, map) {
    expect_runtime_error("expected pair but got integer", "(map + 1)");
    expect_runtime_error("expected pair but got integer", "(for-each + 1)");
}
