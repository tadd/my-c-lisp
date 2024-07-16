#include <stdarg.h>
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
