#ifndef JCC_SLICE_H
#define JCC_SLICE_H

#include <stdbool.h>
#include <stdint.h>
#include <string.h>

#define DECLARE_SLICE(T, Alias, Prefix)                                        \
  typedef struct Alias {                                                       \
    T *data;                                                                   \
    size_t size;                                                               \
  } Alias;

DECLARE_SLICE(char, StringView, sv)

#define EMPTY_SV ((StringView){.data = NULL, .size = 0})

static inline bool sv_equals(StringView *a, StringView *b) {
  return a->size == b->size && memcmp(a->data, b->data, a->size) == 0;
}

static inline uint64_t sv_hash(StringView *sv) {
  uint64_t hash = 14695981039346656037UL;
  for (size_t i = 0; i < sv->size; i++) {
    hash ^= (uint64_t)sv->data[i];
    hash *= 1099511628211;
  }
  return hash;
}

#endif // JCC_SLICE_H
