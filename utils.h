#ifndef UTILS_H
#define UTILS_H

#include <stdlib.h>

#define ATTR(x) __attribute__((x))

#define ATTR_UNUSED ATTR(unused)
#define ATTR_NORETURN ATTR(noreturn)
#define ATTR_MALLOC ATTR(malloc(free))
#define ATTR_FORMAT(f, beg, end) ATTR(format(f, beg, end))

ATTR_NORETURN ATTR_FORMAT(printf, 1, 2) void error(const char *fmt, ...);
ATTR_MALLOC void *xmalloc(size_t size);
ATTR_MALLOC void *xrealloc(void *p, size_t size);

#endif
