#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "utils.h"

void error(const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    fprintf(stderr, "error: ");
    vfprintf(stderr, fmt, ap);
    fprintf(stderr, "\n");
    va_end(ap);
    exit(2);
}

void *xmalloc(size_t size)
{
    void *p = malloc(size);
    if (p == NULL)
        error("malloc(%zu) failed", size);
    return p;
}

void *xrealloc(void *q, size_t size)
{
    void *p = realloc(q, size);
    if (p == NULL)
        error("realloc(ptr, %zu) failed", size);
    return p;
}

char *xstrdup(const char *s)
{
    char *dup = strdup(s);
    if (dup == NULL)
        error("strdup(\"%s\") failed", s);
    return dup;
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
    free(ary->space);
    free(ary);
}

static void darray_maybe_resize(DArray *ary)
{
    if (ary->capacity > ary->size)
        return;
    ary->capacity *= 2;
    ary->space = xrealloc(ary->space, ary->elem_size * ary->capacity);
}

void darray_push(DArray *ary, void *e)
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
