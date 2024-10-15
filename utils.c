#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "utils.h"

void error(const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    fprintf(stderr, "error: ");
    vfprintf(stderr, fmt, ap);
    fprintf(stderr, "\n");
    va_end(ap);
    exit(2);
}

void *xmalloc(size_t size)
{
    void *p = malloc(size);
    if (p == NULL)
        error("%s(%zu) failed", __func__, size);
    return p;
}

static void *xcalloc(size_t nmem, size_t memsize)
{
    void *p = calloc(nmem, memsize);
    if (p == NULL)
        error("%s(%zu, %zu) failed", __func__, nmem, memsize);
    return p;
}

void *xrealloc(void *q, size_t size)
{
    void *p = realloc(q, size);
    if (p == NULL)
        error("%s(%p, %zu) failed", __func__, q, size);
    return p;
}

char *xstrdup(const char *s)
{
    char *dup = strdup(s);
    if (dup == NULL)
        error("%s(\"%s\") failed", __func__, s);
    return dup;
}

enum {
    TABLE_INIT_SIZE = 1,
    TABLE_TOO_MANY_FACTOR = 3,
    TABLE_RESIZE_FACTOR = 2,
};

typedef struct List {
    uint64_t key, value;
    struct List *next;
} List;

struct Table {
    size_t size, body_size;
    List **body;
    TableHashFunc hash;
    TableEqualFunc eq;
};

#if 1
// SEED ^ rapid_mix(SEED ^ secret[0], secret[1]) | SEED = UINT64_C(0xbdd89aa982704029);
static const uint64_t RAPID_SEED0 = UINT64_C(0x763305bbe7bea536);
static const uint64_t RAPID_SECRET0 = UINT64_C(0x2d358dccaa6c78a5),
    RAPID_SECRET1 = UINT64_C(0x8bb84b93962eacc9),
    RAPID_SECRET2 = UINT64_C(0x4b33a62ed433d4a3);
static inline void rapid_mum(uint64_t *a, uint64_t *b)
{
    // r = *a * *b, *a = lower(r), *b = upper(r);
    uint64_t ah = *a >> 32U, al = (uint32_t) *a;
    uint64_t bh = *b >> 32U, bl = (uint32_t) *b;
    uint64_t ch = ah * bh, cm0 = ah * bl, cm1 = bh * al, cl = al * bl;
    uint64_t t = cl + (cm0 << 32U), rl = t + (cm1 << 32U);
    uint64_t rh = ch + (cm0 >> 32U) + (cm1 >> 32U) + (t < cl) + (rl < t);
    *a = rl;
    *b = rh;
}
static inline uint64_t rapid_mix(uint64_t a, uint64_t b) { rapid_mum(&a, &b); return a ^ b; }
#define READ(ty, p) ({ ty v; memcpy(&v, p, sizeof(ty)); (uint64_t) v; })
#define READ64(p) READ(uint64_t, p)
#define READ32(p) READ(uint32_t, p)
static uint64_t rapidhash(const void *key, size_t len)
{
    static const uint64_t S0 = RAPID_SECRET0, S1 = RAPID_SECRET1, S2 = RAPID_SECRET2;
    const uint8_t *p = (uint8_t *) key;
    uint64_t seed = RAPID_SEED0 ^ len, a = 0, b = 0;
    if (len <= 0) {
        // nothing to do
    } else if (len < 4) {
        a = (((uint64_t) p[0]) << 56U) | (((uint64_t) p[len >> 1U]) << 32U) | p[len - 1];
    } else if (len <= 16) {
        const uint8_t *plast = p + len - 4;
        a = (READ32(p) << 32U) | READ32(plast);
        uint64_t d = (len & 24U) >> (len >> 3U);
        b = (READ32(p + d) << 32U) | READ32(plast - d);
    } else {
        size_t i = len;
        if (i > 48) {
            uint64_t see1 = seed, see2 = seed;
            do {
                seed = rapid_mix(READ64(p) ^ S0, READ64(p + 8) ^ seed);
                see1 = rapid_mix(READ64(p + 16) ^ S1, READ64(p + 24) ^ see1);
                see2 = rapid_mix(READ64(p + 32) ^ S2, READ64(p + 40) ^ see2);
                p += 48; i -= 48;
            } while (i >= 48);
            seed ^= see1 ^ see2;
        }
        if (i > 16) {
            seed = rapid_mix(READ64(p) ^ S2, READ64(p + 8) ^ seed ^ S1);
            if (i > 32)
                seed = rapid_mix(READ64(p + 16) ^ S2, READ64(p + 24) ^ seed);
        }
        a = READ64(p + i - 16), b = READ64(p + i - 8);
    }
    a ^= S1, b ^= seed;
    rapid_mum(&a, &b);
    return rapid_mix(a ^ S0 ^ len, b ^ S1);
}
static uint64_t direct_hash(uint64_t x)
{
    return rapidhash(&x, 8);
}

static uint64_t str_hash(uint64_t x)
{
    const char *s = (char *) x;
    return rapidhash(s, strlen(s));
}
#else
static inline uint64_t direct_hash(uint64_t x) // simplified xorshift
{
    x ^= x << 7U;
    x ^= x >> 9U;
    return x;
}

static uint64_t str_hash(uint64_t x) // modified djb2
{
    uint64_t h = 30011;
    for (const char *s = (char *) x; *s != '\0'; s++)
        h = h * 61 + *s;
    return h;
}
#endif

static inline bool direct_equal(uint64_t x, uint64_t y)
{
    return x == y;
}

Table *table_new(void)
{
    return table_new_full(direct_hash, direct_equal);
}

static inline bool str_equal(uint64_t s, uint64_t t)
{
    return strcmp((const char *) s, (const char *) t) == 0;
}

Table *table_new_str(void)
{
    return table_new_full(str_hash, str_equal);
}

Table *table_new_full(TableHashFunc hash, TableEqualFunc eq)
{
    Table *t = xmalloc(sizeof(Table));
    t->body = xcalloc(sizeof(List *), TABLE_INIT_SIZE); // set NULL
    t->size = 0;
    t->body_size = TABLE_INIT_SIZE;
    for (size_t i = 0; i < t->body_size; i++)
        t->body[i] = NULL;
    t->hash = hash != NULL ? hash : direct_hash;
    t->eq = eq;
    return t;
}

static List *list_new(uint64_t key, uint64_t value)
{
    List *l = xmalloc(sizeof(List));
    l->key = key;
    l->value = value;
    l->next = NULL;
    return l;
}

static void list_free(List *l)
{
    for (List *next; l != NULL; l = next) {
        next = l->next;
        free(l);
    }
}

static void list_append(List **p, List *l)
{
    if (*p == NULL) {
        *p = l;
        return;
    }
    List *q;
    for (q = *p; q->next != NULL; q = q->next)
        ;
    q->next = l;
}

void table_free(Table *t)
{
    if (t == NULL)
        return;
    for (size_t i = 0; i < t->body_size; i++)
        list_free(t->body[i]);
    free(t);
}

#if 1
static size_t list_length(List *l)
{
    size_t len = 0;
    for (; l != NULL; l = l->next)
        len++;
    return len;
}

void table_dump(const Table *t)
{
    fprintf(stderr, "size, body_size: %zu, %zu\n", t->size, t->body_size);
    for (size_t i = 0; i < t->body_size; i++) {
        fprintf(stderr, "body[%zu]: %zu\n", i, list_length(t->body[i]));
    }
}
#endif

static inline List **table_body(const Table *t, uint64_t key)
{
    uint64_t i = (*t->hash)(key) % t->body_size;
    return &t->body[i];
}

static inline bool table_too_many_elements(const Table *t)
{
    return t->size > t->body_size * TABLE_TOO_MANY_FACTOR;
}

static size_t next_prime(size_t curr)
{
    static const size_t prime_max = 823117;
    static const size_t primes[] = {
        1, 2, 5, 11, 23, 47, 97, 197, 397, 797, 1597, 3203, 6421,
        12853, 25717, 51437, 102877, 205759, 411527, prime_max,
    };
    static const size_t size = sizeof(primes) / sizeof(primes[0]);

    if (prime_max <= curr)
        goto last;
    for (size_t i = 0; i < size; i++) {
        if (primes[i] > curr)
            return primes[i];
    }
  last:
    return curr*2+1;
}

static void table_resize(Table *t)
{
    const size_t old_body_size = t->body_size;
    List *old_body[old_body_size];
    memcpy(old_body, t->body, sizeof(List *) * t->body_size);
    t->body_size = next_prime(t->body_size);
    t->body = xcalloc(sizeof(List *), t->body_size); // set NULL
    for (size_t i = 0; i < old_body_size; i++) {
        for (List *l = old_body[i], *next; l != NULL; l = next) {
            List **p = table_body(t, l->key);
            next = l->next;
            l->next = NULL;
            list_append(p, l);
        }
    }
}

 // `value` can't be 0
void table_put(Table *t, uint64_t key, uint64_t value)
{
    if (value == 0)
        error("%s: got invalid value == 0", __func__);
    if (table_too_many_elements(t))
        table_resize(t);
    List *l = list_new(key, value);
    List **p = table_body(t, key);
    l->next = *p; // prepend even if the same key exists
    *p = l;
    t->size++;
}

uint64_t table_get(const Table *t, uint64_t key)
{
    const List *l = *table_body(t, key);
    for (; l != NULL; l = l->next) {
        if ((*t->eq)(l->key, key))
            return l->value;
    }
    return 0; // not found
}

void table_merge(Table *dst, const Table *src)
{
    const size_t size = src->body_size;
    for (size_t i = 0; i < size; i++) {
        for (List *l = src->body[i]; l != NULL; l = l->next)
            table_put(dst, l->key, l->value);
    }
}
