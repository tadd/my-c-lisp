#ifndef UTILS_H
#define UTILS_H

#include <stdlib.h>

#define ATTR(x) __attribute__((x))

#define ATTR_UNUSED ATTR(unused)
#define ATTR_NORETURN ATTR(noreturn)
#define ATTR_XMALLOC ATTR(malloc(xfree))
#define ATTR_FORMAT(f, beg, end) ATTR(format(f, beg, end))

ATTR_NORETURN ATTR_FORMAT(printf, 1, 2) void throw(const char *fmt, ...);
void xfree(void *p); // for GC
ATTR_XMALLOC void *xmalloc(size_t size);
ATTR_XMALLOC void *xrealloc(void *p, size_t size);

#endif
