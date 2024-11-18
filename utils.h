#ifndef UTILS_H
#define UTILS_H

#include <stdio.h>
#include <stdlib.h>

#define ATTR(x) __attribute__((x))
#define UNREACHABLE() error("unreachable"), __builtin_unreachable()
#define ATTR_MALLOC ATTR(malloc) ATTR(used)
#define ATTR_XMALLOC ATTR_MALLOC ATTR(returns_nonnull)

ATTR(noreturn) ATTR(format(printf, 1, 2)) void error(const char *fmt, ...);
ATTR_XMALLOC void *xmalloc(size_t size);
ATTR_XMALLOC char *xstrdup(const char *s);
void xfree(void *p);

#define debug(fmt, ...) fprintf(stderr, fmt "\n" __VA_OPT__(,) __VA_ARGS__);

#endif
