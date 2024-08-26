#ifndef JCC_STRING_VIEW_H
#define JCC_STRING_VIEW_H

#include "span.h"

#include <stdint.h>
#include <string.h>

DECLARE_SPAN(char, StringView, sv)

#define EMPTY_SV ((StringView){.data = NULL, .size = 0})

static inline bool sv_equals(StringView *a, StringView *b) {
  if (a->size != b->size) {
    return false;
  }
  for (size_t i = 0; i < a->size; i++) {
    if (a->data[i] != b->data[i]) {
      return false;
    }
  }
  return true;
}

static inline uint64_t sv_hash(StringView *sv) {
  uint64_t hash = 14695981039346656037UL;
  for (size_t i = 0; i < sv->size; i++) {
    hash ^= (uint64_t)sv->data[i];
    hash *= 1099511628211;
  }
  return hash;
}

#endif // JCC_STRING_VIEW_H
