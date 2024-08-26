#include "adt/hash_set.h"

#include <assert.h>
#include <stdlib.h>
#include <string.h>

static void hash_set_maybe_rehash(HashSet *set) {
  if (set->size >= set->capacity / 2) {
    size_t old_capacity = set->capacity;
    const void **old_elements = set->elements;
    set->capacity *= 2;
    set->elements = calloc(set->capacity, sizeof(void *));
    for (size_t i = 0; i < old_capacity; ++i) {
      if (old_elements[i] != NULL) {
        uint64_t h = set->hash(old_elements[i]);
        size_t idx = h & (set->capacity - 1);
        while (set->elements[idx] != NULL) {
          idx = (idx + 1) & (set->capacity - 1);
        }
        set->elements[idx] = old_elements[i];
      }
    }
    free(old_elements);
  }
}

void hs_init(HashSet *set, HashFunc hash, EqualFunc equal) {
  set->size = 0;
  set->capacity = 16;
  set->hash = hash;
  set->equal = equal;
  set->elements = calloc(set->capacity, sizeof(void *));
}

void hs_free(HashSet *set) {
  free(set->elements);
  set->elements = NULL;
  set->capacity = 0;
  set->size = 0;
}

const void *hs_get(const HashSet *set, const void *key) {
  uint64_t h = set->hash(key);
  uint64_t idx = h & (set->capacity - 1);
  while (set->elements[idx] != NULL) {
    if (set->equal(set->elements[idx], key)) {
      return set->elements[idx];
    }
    idx = (idx + 1) & (set->capacity - 1);
  }
  return NULL;
}

const void *hs_insert(HashSet *set, const void *element) {
  assert(element != NULL);
  hash_set_maybe_rehash(set);
  uint64_t h = set->hash(element);
  size_t idx = h & (set->capacity - 1);
  while (set->elements[idx] != NULL) {
    if (set->equal(set->elements[idx], element)) {
      const void *old = set->elements[idx];
      set->elements[idx] = element;
      return old;
    }
    idx = (idx + 1) & (set->capacity - 1);
  }
  ++set->size;
  set->elements[idx] = element;
  return element;
}

const void *hs_try_insert(HashSet *set, const void *element) {
  assert(element != NULL);
  hash_set_maybe_rehash(set);
  uint64_t h = set->hash(element);
  size_t idx = h & (set->capacity - 1);
  while (set->elements[idx] != NULL) {
    if (set->equal(set->elements[idx], element)) {
      return set->elements[idx];
    }
    idx = (idx + 1) & (set->capacity - 1);
  }
  ++set->size;
  set->elements[idx] = element;
  return element;
}

void hs_clear(HashSet *set) {
  set->size = 0;
  memset(set->elements, 0, set->capacity * sizeof(void *));
}
