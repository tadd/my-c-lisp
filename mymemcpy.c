#include <assert.h>
#include <stdint.h>
#include <stdlib.h>

typedef void *memcpy_t(void *restrict dst,
                       const void *restrict src,
                       size_t n);

memcpy_t memcpy_naive, memcpy_duffs,
    memcpy_duffs_8, memcpy_duffs_16, memcpy_duffs_32;

void *memcpy_naive(void *restrict dst, const void *restrict src, size_t n)
{
    uint8_t *d = dst;
    const uint8_t *s = src;
    while (n--)
        *d++ = *s++;
    return dst;
}

void *memcpy_duffs(void *restrict dst, const void *restrict src, size_t n)
{
    uint8_t *d = dst;
    const uint8_t *s = src;
    ssize_t c = (n + 7) / 8;
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wimplicit-fallthrough"
    switch (n % 8) {
    case 0: do { *d++ = *s++;
    case 7:      *d++ = *s++;
    case 6:      *d++ = *s++;
    case 5:      *d++ = *s++;
    case 4:      *d++ = *s++;
    case 3:      *d++ = *s++;
    case 2:      *d++ = *s++;
    case 1:      *d++ = *s++;
            } while (--c > 0);
    }
#pragma GCC diagnostic pop
    return dst;
}

#define TWICE(s) s; s
#define EIGHTS(s) TWICE(TWICE(TWICE(s)))

static inline void copy8(uint8_t *restrict d, const uint8_t *restrict s)
{
    EIGHTS(*d++ = *s++);
}

void *memcpy_duffs_8(void *restrict dst, const void *restrict src, size_t n)
{
    uint8_t *d = dst;
    const uint8_t *s = src;
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wimplicit-fallthrough"
    switch (n % 8) {
    case 7: *d++ = *s++;
    case 6: *d++ = *s++;
    case 5: *d++ = *s++;
    case 4: *d++ = *s++;
    case 3: *d++ = *s++;
    case 2: *d++ = *s++;
    case 1: *d++ = *s++;
    case 0: break;
    }
#pragma GCC diagnostic pop
    size_t c = n / 8;
    for (size_t i = 0; i < c; i++) {
        copy8(d, s);
    }
    return dst;
}

void *memcpy_duffs_16(void *restrict dst, const void *restrict src, size_t n)
{
    uint8_t *d = dst;
    const uint8_t *s = src;
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wimplicit-fallthrough"
    switch (n % 16) {
    case 15: *d++ = *s++;
    case 14: *d++ = *s++;
    case 13: *d++ = *s++;
    case 12: *d++ = *s++;
    case 11: *d++ = *s++;
    case 10: *d++ = *s++;
    case  9: *d++ = *s++;
    case  8: *d++ = *s++;
    case  7: *d++ = *s++;
    case  6: *d++ = *s++;
    case  5: *d++ = *s++;
    case  4: *d++ = *s++;
    case  3: *d++ = *s++;
    case  2: *d++ = *s++;
    case  1: *d++ = *s++;
    case  0: break;
    }
#pragma GCC diagnostic pop
    size_t c = n / 16;
    for (size_t i = 0; i < c; i++) {
        TWICE(EIGHTS(*d++ = *s++));
    }
    return dst;
}

void *memcpy_duffs_32(void *restrict dst, const void *restrict src, size_t n)
{
    uint8_t *d = dst;
    const uint8_t *s = src;
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wimplicit-fallthrough"
    switch (n % 32) {
    case 31: *d++ = *s++;
    case 30: *d++ = *s++;
    case 29: *d++ = *s++;
    case 28: *d++ = *s++;
    case 27: *d++ = *s++;
    case 26: *d++ = *s++;
    case 25: *d++ = *s++;
    case 24: *d++ = *s++;
    case 23: *d++ = *s++;
    case 22: *d++ = *s++;
    case 21: *d++ = *s++;
    case 20: *d++ = *s++;
    case 19: *d++ = *s++;
    case 18: *d++ = *s++;
    case 17: *d++ = *s++;
    case 16: *d++ = *s++;
    case 15: *d++ = *s++;
    case 14: *d++ = *s++;
    case 13: *d++ = *s++;
    case 12: *d++ = *s++;
    case 11: *d++ = *s++;
    case 10: *d++ = *s++;
    case  9: *d++ = *s++;
    case  8: *d++ = *s++;
    case  7: *d++ = *s++;
    case  6: *d++ = *s++;
    case  5: *d++ = *s++;
    case  4: *d++ = *s++;
    case  3: *d++ = *s++;
    case  2: *d++ = *s++;
    case  1: *d++ = *s++;
    case  0: break;
    }
#pragma GCC diagnostic pop
    size_t c = n / 32;
    for (size_t i = 0; i < c; i++) {
        TWICE(TWICE(EIGHTS(*d++ = *s++)));
    }
    return dst;
}

static void *xmalloc(size_t size)
{
    void *p = malloc(size);
    if (p == NULL)
        abort();
    return p;
}

void *memdup_naive(const void *src, size_t n)
{
    void *dst = xmalloc(n);
    uint8_t *d = dst;
    const uint8_t *s = src;
    while (n--)
        *d++ = *s++;
    return dst;
}

static void *xaalloc(size_t alignment, size_t size)
{
    size_t m = size % alignment;
    if (m != 0)
        size += alignment - m;
    void *p = aligned_alloc(alignment, size);
    if (p == NULL)
        abort();
    return p;
}

typedef volatile uint8_t vector64 __attribute__((vector_size(64)));
typedef volatile uint8_t vector32 __attribute__((vector_size(32)));
typedef volatile uint8_t vector16 __attribute__((vector_size(16)));
typedef volatile uint8_t vector8 __attribute__((vector_size(8)));

void *memdup_a64(const void *src, size_t n)
{
    void *dst = xaalloc(64, n);
    uint8_t *d = dst;
    const uint8_t *s = src;
    if (n % 64 == 0) {
        assert((intptr_t)s % 64 == 0);
        const size_t b = 64;
        size_t c = n / b;
        while (c--) {
            *(vector64 *) d = *(vector64 *) s;
            s += b;
            d += b;
        }
    } else if (n % 32 == 0) {
        const size_t b = 32;
        size_t c = n / b;
        while (c--) {
            *(vector32 *) d = *(vector32 *) s;
            s += b;
            d += b;
        }
    } else if (n % 16 == 0) {
        const size_t b = 16;
        size_t c = n / b;
        while (c--) {
            *(vector16 *) d = *(vector16 *) s;
            s += b;
            d += b;
        }
    } else if (n % 8 == 0) {
        const size_t b = 8;
        size_t c = n / b;
        while (c--) {
            *(vector8 *) d = *(vector8 *) s;
            s += b;
            d += b;
        }
    } else {
        while (n--)
            *d++ = *s++;
    }
    return dst;
}
