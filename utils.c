#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

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
        error("%s(%zu) failed", __func__, size);
    return p;
}

void *xcalloc(size_t nmem, size_t memsize)
{
    void *p = calloc(nmem, memsize);
    if (p == NULL)
        error("%s(%zu, %zu) failed", __func__, nmem, memsize);
    return p;
}

void *xrealloc(void *q, size_t size)
{
    void *p = realloc(q, size);
    if (p == NULL)
        error("%s(%p, %zu) failed", __func__, q, size);
    return p;
}

char *xstrdup(const char *s)
{
    char *dup = strdup(s);
    if (dup == NULL)
        error("%s(\"%s\") failed", __func__, s);
    return dup;
}
