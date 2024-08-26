#ifndef JCC_HASH_MAP_H
#define JCC_HASH_MAP_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

typedef struct HashEntry HashEntry;

typedef uint64_t (*HashFunc)(const void *);

typedef bool (*EqualFunc)(const void *, const void *);

typedef struct Hashmap {
  HashFunc hash;
  EqualFunc equal;
  size_t size;
  size_t capacity;
  HashEntry *entries;
} HashMap;

void hm_init(HashMap *map, HashFunc hash, EqualFunc equal);

void hm_free(HashMap *map);

void *hm_get(const HashMap *map, const void *key);

void *hm_set(HashMap *map, const void *key, void *value);

void *hm_try_set(HashMap *map, const void *key, void *value);

void hm_clear(HashMap *map);

#endif // JCC_HASH_MAP_H
