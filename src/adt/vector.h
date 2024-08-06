#ifndef JCC_VECTOR_H
#define JCC_VECTOR_H

#include <assert.h>
#include <stdlib.h>

#define DECLARE_VECTOR(T, Alias, Prefix)                                       \
  typedef struct Alias {                                                       \
    T *data;                                                                   \
    size_t size;                                                               \
    size_t capacity;                                                           \
  } Alias;                                                                     \
                                                                               \
  void Prefix##_init(Alias *v);                                                \
  void Prefix##_with_capacity(Alias *v, size_t capacity);                      \
  void Prefix##_free(Alias *v);                                                \
  void Prefix##_push(Alias *v, T value);                                       \
  T *Prefix##_first(Alias *v);                                                 \
  T *Prefix##_last(Alias *v);

#define DEFINE_VECTOR(T, Alias, Prefix)                                        \
  void Prefix##_init(Alias *v) {                                               \
    v->data = NULL;                                                            \
    v->size = 0;                                                               \
    v->capacity = 0;                                                           \
  }                                                                            \
                                                                               \
  void Prefix##_with_capacity(Alias *v, size_t capacity) {                     \
    capacity = next_power_of_two(capacity);                                    \
    capacity = MAX(capacity, 8);                                               \
    v->data = malloc(capacity * sizeof(T));                                    \
    assert(v->data != NULL);                                                   \
    v->size = 0;                                                               \
    v->capacity = capacity;                                                    \
  }                                                                            \
                                                                               \
  void Prefix##_free(Alias *v) {                                               \
    free(v->data);                                                             \
    v->data = NULL;                                                            \
    v->size = 0;                                                               \
    v->capacity = 0;                                                           \
  }                                                                            \
                                                                               \
  void Prefix##_push(Alias *v, T value) {                                      \
    if (v->size == v->capacity) {                                              \
      v->capacity = GROW_CAP(v->capacity);                                     \
      v->data = realloc(v->data, v->capacity * sizeof(T));                     \
      assert(v->data != NULL);                                                 \
    }                                                                          \
    v->data[v->size++] = value;                                                \
  }                                                                            \
                                                                               \
  T *Prefix##_first(Alias *v) { return v->data; }                              \
                                                                               \
  T *Prefix##_last(Alias *v) { return v->data + v->size - 1; }

#endif // JCC_VECTOR_H
