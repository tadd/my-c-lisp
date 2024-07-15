#ifndef UTILS_H
#define UTILS_H

#include <stdlib.h>

#define ATTR(x) __attribute__((x))
#define UNREACHABLE() __builtin_unreachable()
#define ATTR_UNUSED ATTR(unused)
#define ATTR_NORETURN ATTR(noreturn)
#define ATTR_MALLOC ATTR(malloc) ATTR(used)
#define ATTR_XMALLOC ATTR_MALLOC ATTR(nonnull)
#define ATTR_FORMAT(f, beg, end) ATTR(format(f, beg, end))
#define ATTR_CTOR ATTR(constructor)

ATTR_NORETURN ATTR_FORMAT(printf, 1, 2) void error(const char *fmt, ...);
ATTR_XMALLOC void *xmalloc(size_t size);
ATTR_XMALLOC void *xrealloc(void *p, size_t size);
ATTR_XMALLOC char *xstrdup(const char *s);

#endif
