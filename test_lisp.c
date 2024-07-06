#include <criterion/criterion.h>
#include <criterion/new/assert.h>

#include <stdio.h>
#include <stdlib.h>
#include "utils.h"

Test(darray, new) {
    DArray *a = darray_new(sizeof(int));
    cr_assert(a != NULL);
    darray_free(a);
}

Test(darray, push_and_ref) {
    DArray *a = darray_new(sizeof(int));
    cr_assert(a != NULL);
    int x = 1, y = 21, z = 42;
    darray_push(a, &x);
    darray_push(a, &y);
    darray_push(a, &z);
    
    cr_assert(darray_size(a) == 3);

    int *i = darray_space(a);

    cr_assert(eq(int, i[0], 1));
    cr_assert(eq(int, i[1], 21));
    cr_assert(eq(int, i[2], 42));

    darray_free(a);
}

Test(table, put_and_ref) {
    Table *t = table_new();
    cr_assert(t != NULL);

    table_put(t, "foo");
    table_put(t, "bar");
    table_put(t, "buzz");

    cr_assert(eq(table_get(t, "foo"), 1));
    cr_expect(eq(table_get(t, "bar"), 2));
    cr_expect(eq(table_get(t, "buzz"), 3));
    table_free(t);
}
