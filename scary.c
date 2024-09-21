#include <errno.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "scary.h"

#define NONNULL(p) do { \
        errno = 0; \
        void *q = (p); \
        if (q == NULL) \
            perror("scary"); \
    } while (0)

static inline void *xmalloc(size_t size)
{
    void *p;
    NONNULL(p = malloc(size));
    return p;
}

static inline void *xrealloc(void *p0, size_t size)
{
    void *p;
    NONNULL(p = realloc(p0, size));
    return p;
}

typedef struct {
    size_t capacity, length, elem_size;
    uint8_t space[];
} Scary;

enum {
    SCARY_DISTANCE = offsetof(Scary, space),
    SCARY_INIT = 8,
};

static inline void *opaque(Scary *a)
{
    return a->space;
}

static inline Scary *get(void *p)
{
    uint8_t *bp = p;
    return (Scary *) (bp - SCARY_DISTANCE);
}

void *scary_new(size_t elem_size)
{
    size_t cap = elem_size * SCARY_INIT;
    Scary *ary = xmalloc(sizeof(Scary) + cap);
    ary->capacity = cap;
    ary->length = 0;
    ary->elem_size = elem_size;
    return opaque(ary);
}

void scary_free(void *p)
{
    free(get(p));
}

static void maybe_resize(Scary **pary)
{
    Scary *ary = *pary;
    if (ary->capacity > ary->length * ary->elem_size)
        return;
    ary->capacity *= 2;
    *pary = xrealloc(ary, sizeof(Scary) + ary->capacity);
}

size_t scary_length(const void *p)
{
    const Scary *ary = get((void *) p);
    return ary->length;
}

void scary_push_ref(void *p, const void *elem)
{
    void **pp = (void **) p;
    Scary *ary = get(*pp);
    maybe_resize(&ary);
    uint8_t *sp = ary->space + ary->elem_size * ary->length;
    memcpy(sp, elem, ary->elem_size);
    ary->length++;
    *pp = opaque(ary);
}

#define DEF_PUSH_VARIANT2(type, suffix) \
    void scary_push_##suffix(void *p, type elem) \
    { \
        type tmp = elem; \
        scary_push_ref(p, &tmp); \
    }
#define DEF_PUSH_VARIANT(type) DEF_PUSH_VARIANT2(type##_t, type)

DEF_PUSH_VARIANT(int8)
DEF_PUSH_VARIANT(int16)
DEF_PUSH_VARIANT(int32)
DEF_PUSH_VARIANT(int64)
DEF_PUSH_VARIANT(uint8)
DEF_PUSH_VARIANT(uint16)
DEF_PUSH_VARIANT(uint32)
DEF_PUSH_VARIANT(uint64)
DEF_PUSH_VARIANT2(const void *, ptr)

void scary_pop(void *p)
{
    Scary *ary = get(p);
    ary->length--; // do not shrink for speed
}

void *scary_dup(void *p)
{
    Scary *ary = get(p);
    Scary *dup = xmalloc(sizeof(Scary) + ary->capacity);
    *dup = *ary;
    memcpy(dup->space, ary->space, ary->capacity);
    return opaque(dup);
}
