#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "utils.h"

void throw(const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    fprintf(stderr, "error: ");
    vfprintf(stderr, fmt, ap);
    fprintf(stderr, "\n");
    exit(2);
}

void *xmalloc(size_t size)
{
    void *p = malloc(size);
    if (p == NULL)
        throw("malloc %zu bytes failed", size);
    return p;
}

void *xrealloc(void *p, size_t size)
{
    p = realloc(p, size);
    if (p == NULL)
        throw("realloc to %zu bytes failed", size);
    return p;
}

void xfree(void *p)
{
    free(p);
}

struct DArray {
    void *space;
    uint64_t capacity, size, elem_size;
};

enum {
    DARRAY_INIT = 1,
};

DArray *darray_new(size_t elem_size)
{
    DArray *ary = xmalloc(sizeof(DArray));
    ary->capacity = elem_size * DARRAY_INIT;
    ary->size = 0;
    ary->elem_size = elem_size;
    ary->space = xmalloc(elem_size);
    return ary;
}

void darray_free(DArray *ary)
{
    xfree(ary->space);
    xfree(ary);
}

static void darray_maybe_resize(DArray *ary)
{
    if (ary->capacity > ary->size)
        return;
    ary->capacity *= 2;
    ary->space = xrealloc(ary->space, ary->elem_size * ary->capacity);
}

void darray_put(DArray *ary, void *e)
{
    darray_maybe_resize(ary);
    uint8_t *sp = ary->space;
    size_t last = ary->elem_size * ary->size;
    memcpy(sp + last, e, ary->elem_size);
    ary->size++;
}

size_t darray_size(const DArray *ary)
{
    return ary->size;
}

void *darray_space(const DArray *ary)
{
    return ary->space;
}
