#ifndef UTILS_H
#define UTILS_H

#include <stdlib.h>

#define ATTR(x) [[gnu::x]]
#define UNREACHABLE() error("unreachable"), __builtin_unreachable()
#define ATTR_MALLOC ATTR(malloc) [[nodiscard]]
#define ATTR_UNUSED [[maybe_unused]]
#define ATTR_XMALLOC ATTR_MALLOC ATTR(returns_nonnull)

ATTR(noreturn) ATTR(format(printf, 1, 2)) void error(const char *fmt, ...);
ATTR_XMALLOC void *xmalloc(size_t size);
ATTR_XMALLOC void *xrealloc(void *p, size_t size);
ATTR_XMALLOC char *xstrdup(const char *s);

#endif
