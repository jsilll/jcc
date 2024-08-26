#ifndef JCC_HASH_SET_H
#define JCC_HASH_SET_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

typedef uint64_t (*HashFunc)(const void *);

typedef bool (*EqualFunc)(const void *, const void *);

typedef struct HashSet {
  HashFunc hash;
  EqualFunc equal;
  size_t size;
  size_t capacity;
  const void **elements;
} HashSet;

void hs_init(HashSet *set, HashFunc hash, EqualFunc equal);

void hs_free(HashSet *set);

const void *hs_get(const HashSet *set, const void *key);

const void *hs_insert(HashSet *set, const void *element);

const void *hs_try_insert(HashSet *set, const void *element);

void hs_clear(HashSet *set);

#endif // JCC_HASH_SET_H
