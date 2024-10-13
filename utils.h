#ifndef UTILS_H
#define UTILS_H

#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#define ATTR(x) __attribute__((x))
#define UNREACHABLE() error("unreachable"), __builtin_unreachable()
#define ATTR_MALLOC ATTR(malloc) ATTR(used)
#define ATTR_XMALLOC ATTR_MALLOC ATTR(returns_nonnull)

ATTR(noreturn) ATTR(format(printf, 1, 2)) void error(const char *fmt, ...);
ATTR_XMALLOC void *xmalloc(size_t size);
ATTR_XMALLOC void *xrealloc(void *p, size_t size);
ATTR_XMALLOC char *xstrdup(const char *s);

typedef struct IntTable IntTable;
IntTable *int_table_new(void);
void int_table_free(IntTable *t);
void int_table_put(IntTable *t, uint64_t key, uint64_t val); // `val` can't be 0
uint64_t int_table_get(const IntTable *t, uint64_t key);
void int_table_merge(IntTable *dst, const IntTable *src);

#define debug(fmt, ...) fprintf(stderr, fmt "\n" __VA_OPT__(,) __VA_ARGS__);

#endif
