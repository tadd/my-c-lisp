#include <stdarg.h>
#include <stddef.h>
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

char *xstrdup(const char *s)
{
    char *dup = xmalloc(strlen(s) + 1);
    strcpy(dup, s);
    return dup;
}

struct DArray {
    uint64_t capacity, size, elem_size;
    uint8_t space[];
};

enum {
    DARRAY_DISTANCE = offsetof(DArray, space),
    DARRAY_INIT = 1,
};

static inline void *darray_opaque(DArray *a)
{
    return a->space;
}

static inline DArray *darray_unopaque(void *p)
{
    uint8_t *bp = p;
    return (DArray *) (bp - DARRAY_DISTANCE);
}


void *darray_new(size_t elem_size)
{
    uint64_t cap = elem_size * DARRAY_INIT;
    DArray *ary = xmalloc(sizeof(DArray) + cap);
    ary->capacity = cap;
    ary->size = 0;
    ary->elem_size = elem_size;
    return darray_opaque(ary);
}

void darray_free(void *p)
{
    DArray *ary = darray_unopaque(p);
    xfree(ary);
}

static void darray_maybe_resize(DArray **pary)
{
    DArray *ary = *pary;
    if (ary->capacity > ary->size)
        return;
    ary->capacity *= 2;
    *pary = xrealloc(ary, ary->elem_size * ary->capacity);
}

void darray_push(void *p, void *e)
{
    DArray *ary = darray_unopaque(p);
    darray_maybe_resize(&ary);
    uint8_t *sp = ary->space + ary->elem_size * ary->size;
    memcpy(sp, e, ary->elem_size);
    ary->size++;
}

size_t darray_size(const void *p)
{
    const DArray *ary = darray_unopaque((void *)p);
    return ary->size;
}

void *darray_space(const void *p)
{
    return (void *)p;
}

Table *table_new(void)
{
    return darray_new(sizeof(char *));
}

void table_free(Table *t)
{
    DArray *a = (DArray *) t;
    const size_t size = darray_size(a);
    char **space = darray_space(a);
    for (size_t i = 0; i < size; i++) {
        xfree(space[i]);
    }
    darray_free(t);
}

void table_put(Table *t, const char *key)
{
    DArray *a = (DArray *) t;
    char *dup = xstrdup(key);
    darray_push(a, &dup);
}

uint64_t table_get(Table *t, const char *key)
{
    DArray *a = (DArray *) t;
    char **space = darray_space(a);
    const size_t size = darray_size(a);
    for (uint64_t i = 0; i < size; i++) {
        if (strcmp(space[i], key) == 0)
            return i+1; // never be zero
    }
    return 0;
}

size_t table_size(const Table *t)
{
    DArray *a = (DArray *) t;
    return darray_size(a);
}
