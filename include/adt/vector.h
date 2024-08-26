#ifndef JCC_VECTOR_H
#define JCC_VECTOR_H

#include <assert.h>
#include <stdlib.h>

#define DECLARE_VEC(T, Alias, Prefix)                                          \
  typedef struct Alias {                                                       \
    T *data;                                                                   \
    size_t size;                                                               \
    size_t capacity;                                                           \
  } Alias;                                                                     \
                                                                               \
  static inline void Prefix##_init(Alias *v) {                                 \
    v->data = NULL;                                                            \
    v->size = 0;                                                               \
    v->capacity = 0;                                                           \
  }                                                                            \
                                                                               \
  static inline void Prefix##_free(Alias *v) {                                 \
    free(v->data);                                                             \
    v->data = NULL;                                                            \
    v->size = 0;                                                               \
    v->capacity = 0;                                                           \
  }                                                                            \
                                                                               \
  static inline void Prefix##_with_capacity(Alias *v, size_t capacity) {       \
    capacity = next_power_of_two(capacity);                                    \
    capacity = MAX(capacity, 8);                                               \
    v->data = malloc(capacity * sizeof(T));                                    \
    assert(v->data != NULL);                                                   \
    v->size = 0;                                                               \
    v->capacity = capacity;                                                    \
  }                                                                            \
                                                                               \
  static inline void Prefix##_push(Alias *v, T value) {                        \
    if (v->size == v->capacity) {                                              \
      v->capacity = GROW_CAP(v->capacity);                                     \
      void *tmp = realloc(v->data, v->capacity * sizeof(T));                   \
      assert(tmp != NULL);                                                     \
      v->data = tmp;                                                           \
    }                                                                          \
    v->data[v->size++] = value;                                                \
  }                                                                            \
                                                                               \
  static inline T *Prefix##_first(Alias *v) { return v->data; }                \
                                                                               \
  static inline T *Prefix##_last(Alias *v) { return v->data + v->size - 1; }

#endif // JCC_VECTOR_H
