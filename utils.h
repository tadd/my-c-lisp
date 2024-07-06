#ifndef UTILS_H
#define UTILS_H

#include <stdlib.h>

#define ATTR(x) __attribute__((x))
#define UNREACHABLE() error("unreachable"), __builtin_unreachable()
#define ATTR_MALLOC ATTR(malloc) ATTR(used)
#define ATTR_XMALLOC ATTR_MALLOC ATTR(returns_nonnull)

ATTR(noreturn) ATTR(format(printf, 1, 2)) void error(const char *fmt, ...);
ATTR_XMALLOC void *xmalloc(size_t size);
ATTR_XMALLOC void *xrealloc(void *p, size_t size);
ATTR_XMALLOC char *xstrdup(const char *s);

typedef struct DArray DArray;
DArray *darray_new(size_t size);
void darray_free(DArray *ary);
void darray_push(DArray *ary, void *e);
size_t darray_size(const DArray *ary);
void *darray_space(const DArray *ary);

#endif
