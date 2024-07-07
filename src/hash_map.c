#include "hash_map.h"

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

void hash_map_init(HashMap *map, size_t capacity, HashFunc hash,
                   EqualFunc equal) {
  capacity = next_power_of_two(capacity);
  capacity = MAX(capacity, 16);
  map->hash = hash;
  map->equal = equal;
  map->size = 0;
  map->capacity = capacity;
  map->entries = calloc(capacity, sizeof(HashEntry));
}

void hash_map_free(HashMap *map) {
  free(map->entries);
  map->entries = NULL;
  map->capacity = 0;
  map->size = 0;
}

void *hash_map_get(const HashMap *map, const void *key) {
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

void *hash_map_set(HashMap *map, const void *key, void *value) {
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

void *hash_map_try_set(HashMap *map, const void *key, void *value) {
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

void hash_map_clear(HashMap *map) {
  memset(map->entries, 0, map->capacity * sizeof(HashEntry));
  map->size = 0;
}
