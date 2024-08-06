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

void hash_set_init(HashSet *set, size_t capacity, HashFunc hash,
                   EqualFunc equal);
void hash_set_free(HashSet *set);
const void *hash_set_get(const HashSet *set, const void *key);
const void *hash_set_insert(HashSet *set, const void *element);
void hash_set_clear(HashSet *set);

#endif // JCC_HASH_SET_H
