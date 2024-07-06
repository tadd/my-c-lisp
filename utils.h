#ifndef UTILS_H
#define UTILS_H

#include <stdint.h>
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
void *darray_new(size_t size);
void darray_free(void *ary);
void darray_push(void *ary, void *e);
size_t darray_size(const void *ary);
void *darray_space(const void *ary);

typedef struct DArray Table; // !!
Table *table_new(void); // string->id(uint) table
void table_free(Table *t);
void table_put(Table *t, const char *key);
uint64_t table_get(Table *t, const char *key);
size_t table_size(const Table *t);

#endif
