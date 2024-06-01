#ifndef CC_HASH_MAP_H
#define CC_HASH_MAP_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

/// An entry in a hash map.
typedef struct HashEntry HashEntry;

/// The hashing function for a hash map.
typedef uint64_t (*HashFunc)(const void *);

/// The equality function for a hash map.
typedef bool (*EqualFunc)(const void *, const void *);

/// A hash map.
typedef struct Hashmap {
  HashFunc hash;
  EqualFunc equal;
  size_t length;
  size_t capacity;
  HashEntry *entries;
} HashMap;

/// Creates a hash map with the given capacity, hash function, and equality
/// function.
/// @param capacity The initial capacity of the hash map.
/// @param hash The hash function for the hash map.
/// @param equal The equality function for the hash map.
HashMap hash_map_create(size_t capacity, HashFunc hash, EqualFunc equal);

/// Gets the value associated with the given key in the hash map.
/// @param map The hash table.
/// @param key The key to search for.
void *hash_map_get(const HashMap *table, const void *key);

/// Sets the value associated with the given key in the hash map.
/// @param map The hash table.
/// @param key The key to set.
void *hash_map_set(HashMap *table, const void *key, void *value);

/// Tries to set the value associated with the given key in the hash map.
/// @param map The hash table.
/// @param key The key to set.
void *hash_map_try_set(HashMap *table, const void *key, void *value);

/// Removes the value associated with the given key in the hash map.
/// @param map The hash table.
/// @param key The key to remove.
void hash_map_clear(HashMap *table);

/// Destroys the hash map.
/// @param map The hash table.
void hash_map_destroy(HashMap *table);

#endif // CC_HASH_MAP_H
