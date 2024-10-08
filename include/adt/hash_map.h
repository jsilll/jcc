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

void hash_map_init(HashMap *map, size_t capacity, HashFunc hash,
                   EqualFunc equal);
void hash_map_free(HashMap *map);
void *hash_map_get(const HashMap *map, const void *key);
void *hash_map_set(HashMap *map, const void *key, void *value);
void *hash_map_try_set(HashMap *map, const void *key, void *value);
void hash_map_clear(HashMap *map);

#endif // JCC_HASH_MAP_H
