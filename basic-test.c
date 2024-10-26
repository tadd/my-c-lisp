#include <stdlib.h>
#include <string.h>

#include <criterion/criterion.h>
#include <criterion/new/assert.h>

#include "lisp.h"
#include "utils.h"

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

Test(lisp, printing) {
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

Test(lisp, parse_int) {
    expect_vint_eq_parsed(42, "42");
    expect_vint_eq_parsed(-42, "-42");
}

Test(lisp, parse_nil) {
    cr_expect(parse_expr_string("()") == Qnil);
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

#define caaaar(x) (car(car(car(car(x)))))
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
    expect_list_eq_parsed(list(value_of_symbol("lambda"), Qnil, V(42), Qundef),
                          "(lambda () 42)");
}

Test(lisp, parse_broken) {
    expect_parse_error("got 'EOF'", "(");
    expect_parse_error("got 'EOF'", "'");
}

Test(lisp, parse_error_line_column) {
    expect_parse_error("<inline>:1:3: ", "(1");
    expect_parse_error("<inline>:2:3: ", "\n(1");
    expect_parse_error("<inline>:2:5: ", "()\n()(1");
}

Test(lisp, runtime_error_line_column) {
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

Test(lisp, div0) {
    expect_runtime_error("divided by zero", "(/ 42 0)");
}

Test(lisp, modulo) {
    expect_runtime_error("divided by zero", "(modulo 13 0)");
}

Test(lisp, unbound_variable) {
    expect_runtime_error("unbound variable: x", "x");
    expect_runtime_error("unbound variable: x", "(+ x 2)");
}

Test(lisp, if) {
    expect_runtime_error("2..3 but got 1", "(if #f)");
    expect_runtime_error("2..3 but got 4", "(if #f 1 2 3)");
}

Test(lisp, set) {
    expect_runtime_error("unbound variable: x", "(begin (set! x 42) x)");
}

Test(lisp, let) {
    expect_runtime_error("but got 1", "(let ((x 42)))");
    expect_runtime_error("but got 1", "(let ((x 42) (y 100)))");
}

Test(lisp, applicable) {
    expect_runtime_error("expected procedure", "(1 1)");
    expect_runtime_error("expected procedure", "(() 1)");
}

Test(lisp, map) {
    expect_runtime_error("expected pair but got integer", "(map + 1)");
    expect_runtime_error("expected pair but got integer", "(for-each + 1)");
}

Test(table, get_put) {
    Table *t = table_new();
    table_put(t, 1, 100);
    cr_assert(eq(int, 100, table_get(t, 1)));
    table_put(t, 2, 200);
    table_put(t, 3, 300);
    table_put(t, 4, 400);
    cr_assert(eq(int, 100, table_get(t, 1)));
    cr_assert(eq(int, 200, table_get(t, 2)));
    cr_assert(eq(int, 300, table_get(t, 3)));
    cr_assert(eq(int, 400, table_get(t, 4)));
    table_put(t, 1, 42);
    cr_assert(eq(int, 42, table_get(t, 1)));
    cr_assert(eq(int, 200, table_get(t, 2)));
    cr_assert(eq(int, 300, table_get(t, 3)));
    cr_assert(eq(int, 400, table_get(t, 4)));

    for (int i = 1; i <= 17; i++)
        table_put(t, i, i*10000000);
    for (int i = 1; i <= 17; i++)
        cr_assert(eq(int, i*10000000, table_get(t, i)));

    table_free(t);
}

Test(table, merge) {
    Table *t = table_new();
    for (size_t i = 1; i <= 11; i++)
        table_put(t, i, i*13);
    Table *u = table_new();
    for (size_t i = 5; i <= 17; i++)
        table_put(u, i, i*19);
    table_merge(t, u);

    for (int i = 1; i < 5; i++)
        cr_assert(eq(sz, i*13, table_get(t, i)));
    for (int i = 5; i <= 17; i++)
        cr_assert(eq(sz, i*19, table_get(t, i)));

    table_free(t);
    table_free(u);
}

Test(table, merge_precedence) {
    Table *t = table_new();
    for (size_t i = 1; i <= 3; i++) {
        table_put(t, 1, i*10);
        cr_assert(eq(sz, i*10, table_get(t, 1)));
    }

    Table *u = table_new();
    for (size_t i = 1; i <= 3; i++) {
        table_put(u, 1, i*20);
        cr_assert(eq(sz, i*20, table_get(u, 1)));
    }

    table_merge(t, u);
    // the latest entry of `u` should take precedence in `t` too
    cr_assert(eq(sz, 60, table_get(t, 1)));

    table_free(t);
    table_free(u);
}

static inline uint64_t keydup(uint64_t s)
{
    return (uint64_t) xstrdup((char *) s);
}

Test(table, string_keys) {
    uint64_t k_foo = (uint64_t) "foo";
    uint64_t k_bar = (uint64_t) "bar";

    Table *t = table_new_str();
    table_put(t, keydup(k_foo), 12);
    table_put(t, keydup(k_bar), 34);
    table_put(t, keydup(k_foo), 56);

    cr_assert(eq(int, 56, table_get(t, k_foo)));
    cr_assert(eq(int, 34, table_get(t, k_bar)));

    table_free(t);
}

Test(table, inherit) {
    uint64_t k_foo = (uint64_t) "foo";
    uint64_t k_bar = (uint64_t) "bar";
    uint64_t k_baz = (uint64_t) "baz";

    Table *t = table_new_str();
    table_put(t, keydup(k_foo), 12);
    table_put(t, keydup(k_bar), 34);

    Table *u = table_inherit(t);
    table_put(u, keydup(k_baz), 56);

    cr_assert(eq(int, 12, table_get(u, k_foo)));
    cr_assert(eq(int, 34, table_get(u, k_bar)));
    cr_assert(eq(int, 56, table_get(u, k_baz)));

    cr_assert(eq(int, 12, table_get(t, k_foo)));
    cr_assert(eq(int, 34, table_get(t, k_bar)));
    cr_assert(eq(int,  0, table_get(t, k_baz)));

    table_free(u);
    table_free(t);
}

Test(table, inheritance_with_huge_child) {
    Table *t = table_new();
    for (long i = 1; i < 10; i++)
        table_put(t, i, i*13);

    Table *u = table_inherit(t);
    for (long i = 1; i < 100; i++)
        table_put(u, i, i*17);

    cr_assert(eq(int, 13, table_get(t, 1)));

    table_free(u);
    table_free(t);
}

Test(table, inheritance_before_parent_put) {
    Table *t = table_new();
    Table *u = table_inherit(t);

    for (long i = 1; i < 100; i++)
        table_put(t, i, i*17);

    cr_assert(eq(int, 17, table_get(t, 1)));
    cr_assert(eq(int, 17, table_get(u, 1)));

    table_free(u);
    table_free(t);
}

Test(table, inheritance_and_put) {
    Table *t = table_new();
    for (long i = 1; i <= 10; i++)
        table_put(t, i, i*13); // put to parent

    Table *u = table_inherit(t);
    for (long i = 11; i <= 20; i++)
        table_put(t, i, i*17); // put to parent after inheritance

    for (long i = 21; i <= 30; i++)
        table_put(u, i, i*10); // put to child after inheritance

    cr_assert(eq(int,  13, table_get(t,  1)));
    cr_assert(eq(int, 340, table_get(t, 20)));
    cr_assert(eq(int,   0, table_get(t, 30)));

    cr_assert(eq(int,  13, table_get(u,  1)));
    cr_assert(eq(int, 340, table_get(u, 20)));
    cr_assert(eq(int, 300, table_get(u, 30)));

    table_free(u);
    table_free(t);
}

Test(table, set) {
    Table *t = table_new();
    table_put(t, 1, 123);
    table_put(t, 2, 456);

    bool s1 = table_set(t, 2, 789); // overwrite!
    bool s2 = table_set(t, 3, 210); // ignored

    cr_assert(s1);
    cr_assert(not(s2));
    cr_assert(eq(int, 123, table_get(t, 1)));
    cr_assert(eq(int, 789, table_get(t, 2)));
    cr_assert(eq(int, 0, table_get(t, 3))); // still not found!

    table_free(t);
}

Test(table, set_or_put) {
    Table *t = table_new();
    table_put(t, 1, 123);
    table_put(t, 2, 456);

    bool s1 = table_set_or_put(t, 2, 789); // overwrite!
    bool s2 = table_set_or_put(t, 3, 210); // just add

    cr_assert(s1);
    cr_assert(not(s2));
    cr_assert(eq(int, 123, table_get(t, 1)));
    cr_assert(eq(int, 789, table_get(t, 2)));
    cr_assert(eq(int, 210, table_get(t, 3)));

    table_free(t);
}
