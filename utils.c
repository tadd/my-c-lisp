#include <stdarg.h>
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

static inline uint64_t direct_hash(uint64_t x) // simplified xorshift
{
    x ^= x << 7U;
    x ^= x >> 9U;
    return x;
}

static inline bool direct_equal(uint64_t x, uint64_t y)
{
    return x == y;
}

Table *table_new(void)
{
    return table_new_full(direct_hash, direct_equal);
}

static uint64_t str_hash(uint64_t x) // modified djb2
{
    uint64_t h = 30011;
    for (const char *s = (char *) x; *s != '\0'; s++)
        h = h * 61 + *s;
    return h;
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
    t->body = xmalloc(sizeof(List *) * TABLE_INIT_SIZE);
    t->size = 0;
    t->body_size = TABLE_INIT_SIZE;
    for (size_t i = 0; i < t->body_size; i++)
        t->body[i] = NULL;
    t->hash = hash ? hash : direct_hash;
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
    free(t->body);
    free(t);
}

#if 0
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

// "next" is twice or more larger than `curr`
static size_t next_prime(size_t curr)
{
    static const size_t prime_max = 823117;
    static const size_t primes[] = {
        1, 2, 5, 11, 23, 47, 97, 197, 397, 797, 1597, 3203, 6421,
        12853, 25717, 51437, 102877, 205759, 411527, prime_max,
    };
    if (prime_max <= curr)
        goto last;
    static const size_t size = sizeof(primes) / sizeof(primes[0]);
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
    t->body = xcalloc(sizeof(List *), t->body_size);
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
