#include "adt/hash_map.h"

#include <assert.h>
#include <stdlib.h>
#include <string.h>

typedef struct HashEntry {
  const void *key;
  void *value;
} HashEntry;

static void hash_map_maybe_rehash(HashMap *map) {
  if (map->size >= map->capacity / 2) {
    size_t old_capacity = map->capacity;
    HashEntry *old_entries = map->entries;
    map->capacity = map->capacity * 2;
    map->entries = calloc(map->capacity, sizeof(HashEntry));
    for (size_t i = 0; i < old_capacity; ++i) {
      if (old_entries[i].key != NULL) {
        uint64_t h = map->hash(old_entries[i].key);
        size_t idx = (h & (map->capacity - 1));
        while (map->entries[idx].key != NULL) {
          idx = (idx + 1) & (map->capacity - 1);
        }
        map->entries[idx] = old_entries[i];
      }
    }
    free(old_entries);
  }
}

void hm_init(HashMap *map, HashFunc hash, EqualFunc equal) {
  map->size = 0;
  map->capacity = 16;
  map->hash = hash;
  map->equal = equal;
  map->entries = calloc(map->capacity, sizeof(HashEntry));
}

void hm_free(HashMap *map) {
  free(map->entries);
  map->entries = NULL;
  map->capacity = 0;
  map->size = 0;
}

void *hm_get(const HashMap *map, const void *key) {
  uint64_t h = map->hash(key);
  size_t idx = (h & (map->capacity - 1));
  while (map->entries[idx].key != NULL) {
    if (map->equal(map->entries[idx].key, key)) {
      return map->entries[idx].value;
    }
    idx = (idx + 1) & (map->capacity - 1);
  }
  return NULL;
}

// NOLINTNEXTLINE
void *hm_set(HashMap *map, const void *key, void *value) {
  assert(key != NULL);
  hash_map_maybe_rehash(map);
  uint64_t h = map->hash(key);
  size_t idx = (h & (map->capacity - 1));
  while (map->entries[idx].key != NULL) {
    if (map->equal(map->entries[idx].key, key)) {
      void *old = map->entries[idx].value;
      map->entries[idx].value = value;
      return old;
    }
    idx = (idx + 1) & (map->capacity - 1);
  }
  ++map->size;
  map->entries[idx].key = key;
  map->entries[idx].value = value;
  return NULL;
}

// NOLINTNEXTLINE
void *hm_try_set(HashMap *map, const void *key, void *value) {
  assert(key != NULL);
  hash_map_maybe_rehash(map);
  uint64_t h = map->hash(key);
  size_t idx = (h & (map->capacity - 1));
  while (map->entries[idx].key != NULL) {
    if (map->equal(map->entries[idx].key, key)) {
      return map->entries[idx].value;
    }
    idx = (idx + 1) & (map->capacity - 1);
  }
  ++map->size;
  map->entries[idx].key = key;
  map->entries[idx].value = value;
  return value;
}

void hm_clear(HashMap *map) {
  memset(map->entries, 0, map->capacity * sizeof(HashEntry));
  map->size = 0;
}
