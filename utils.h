#ifndef UTILS_H
#define UTILS_H

#include <stdlib.h>

#define ATTR_UNUSED [[unused]]
#define ATTR_NORETURN [[gnu::noreturn]]
#define ATTR_MALLOC [[gnu::malloc]] [[gnu::used]]
#define ATTR_XMALLOC ATTR_MALLOC [[gnu::nonnull]]
#define ATTR_FORMAT(f, beg, end) [[gnu::format(f, beg, end)]]

ATTR_NORETURN ATTR_FORMAT(printf, 1, 2) void error(const char *fmt, ...);
ATTR_XMALLOC void *xmalloc(size_t size);
ATTR_XMALLOC void *xrealloc(void *p, size_t size);
ATTR_XMALLOC char *xstrdup(const char *s);

#endif
