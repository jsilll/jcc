#ifndef CC_HASH_SET_H
#define CC_HASH_SET_H

#include <stdbool.h>
#include <stdint.h>
#include <stddef.h>

/// The hashing function for a hash set.
typedef uint64_t (*HashFunc)(const void *);

/// The equality function for a hash set.
typedef bool (*EqualFunc)(const void *, const void *);

/// A hash set.
typedef struct HashSet {
  HashFunc hash;
  EqualFunc equal;
  size_t length;
  size_t capacity;
  const void **elements;
} HashSet;

/// Creates a hash set with the given capacity, hash function, and equality
/// function.
/// @param capacity The initial capacity of the hash set.
/// @param hash The hash function for the hash set.
/// @param equal The equality function for the hash set.
HashSet hash_set_create(size_t capacity, HashFunc hash, EqualFunc equal);

/// Gets the element associated with the given key in the hash set.
/// @param set The hash set.
/// @param key The key to search for.
const void *hash_set_get(const HashSet *set, const void *key);

/// Inserts the element into the hash set.
/// @param set The hash set.
/// @param element The element to insert.
const void *hash_set_insert(HashSet *set, const void *element);

/// Removes the element associated with the given key in the hash set.
/// @param set The hash set.
void hash_set_clear(HashSet *set);

/// Destroys the hash set.
/// @param set The hash set.
void hash_set_destroy(HashSet *set);

#endif // CC_HASH_SET_H
