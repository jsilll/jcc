#ifndef JCC_BASE_H
#define JCC_BASE_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

static inline uint32_t digit_count(uint32_t n) {
  uint32_t digits = 1;
  for (uint32_t i = n; i >= 10; i /= 10) {
    ++digits;
  }
  return digits;
}

static inline size_t next_power_of_two(size_t n) {
  --n;
  n |= n >> 1;
  n |= n >> 2;
  n |= n >> 4;
  n |= n >> 8;
  n |= n >> 16;
  n |= n >> 32;
  ++n;
  return n;
}

static inline void line_number_fmt(char *s, size_t n, uint32_t digits) {
  snprintf(s, n, "%%%dd | ", digits);
}

#define GENERATE_ENUM(ENUM) ENUM,

#define GENERATE_STRING(STRING) #STRING,

#define DEBUG(msg)                                                             \
  fprintf(stderr, "[%s:%d] debug: %s\n", __FILE__, __LINE__, msg)

#define DEBUGF(fmt, ...)                                                       \
  fprintf(stderr, "[%s:%d] debug: " fmt "\n", __FILE__, __LINE__, __VA_ARGS__)

#define TODO(msg)                                                              \
  do {                                                                         \
    fprintf(stderr, "[%s:%d] todo: %s\n", __FILE__, __LINE__, msg);            \
    exit(EXIT_FAILURE);                                                        \
  } while (0)

#define PANIC(msg)                                                             \
  do {                                                                         \
    fprintf(stderr, "[%s:%d] panic: %s\n", __FILE__, __LINE__, msg);           \
    exit(EXIT_FAILURE);                                                        \
  } while (0)

#define RETURN_NULL_IF_NULL(x)                                                 \
  do {                                                                         \
    if ((x) == NULL) {                                                         \
      return NULL;                                                             \
    }                                                                          \
  } while (0)

#define KB(x) ((x) * 1024)

#define MB(x) ((x) * 1024 * 1024)

#define GB(x) ((x) * 1024 * 1024 * 1024)

#define MAX(a, b) ((a) > (b) ? (a) : (b))

#define MIN(a, b) ((a) < (b) ? (a) : (b))

#define CLAMP(x, low, high)                                                    \
  ((x) < (low) ? (low) : ((x) > (high) ? (high) : (x)))

#define IS_POW2(x) (((x) & ((x)-1)) == 0)

#define IS_ASCII(c) ((c) >= 0 && (c) <= 127)

#define ALIGN_DOWN(x, align) ((x) & ~((align)-1))

#define ALIGN_UP(x, align) (((x) + (align)-1) & ~((align)-1))

#define IS_ALIGNED(x, align) (((x) & ((align)-1)) == 0)

#define GROW_CAP(capacity) ((capacity) < 8 ? 8 : (capacity) * 2)

#define DECLARE_ENUM_WITH_REPR(Alias, Enum)                                    \
  typedef enum Alias { Enum(GENERATE_ENUM) } Alias;                            \
  extern const char *const Alias##_Repr[];

#define DEFINE_ENUM_WITH_REPR(Alias, Enum)                                     \
  const char *const Alias##_Repr[] = {Enum(GENERATE_STRING)};

#define DECLARE_SLICE(T, Alias, Prefix)                                        \
  typedef struct Alias {                                                       \
    T *data;                                                                   \
    uint32_t size;                                                             \
  } Alias;

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
    v->data = malloc(capacity * sizeof(T));                                    \
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
    }                                                                          \
    v->data[v->size++] = value;                                                \
  }                                                                            \
                                                                               \
  T *Prefix##_first(Alias *v) { return v->data; }                              \
                                                                               \
  T *Prefix##_last(Alias *v) { return v->data + v->size - 1; }

#define DECLARE_SMALL_VECTOR(T, N, Alias, Prefix)                              \
  typedef struct Alias {                                                       \
    T stack[N];                                                                \
    T *heap;                                                                   \
    uint32_t size;                                                             \
    uint32_t capacity;                                                         \
  } Alias;                                                                     \
                                                                               \
  void Prefix##_init(Alias *v);                                                \
  void Prefix##_free(Alias *v);                                                \
  void Prefix##_push(Alias *v, T value);                                       \
  T *Prefix##_get(Alias *v, uint32_t idx);

#define DEFINE_SMALL_VECTOR(T, N, Alias, Prefix)                               \
  void Prefix##_init(Alias *v) {                                               \
    v->heap = NULL;                                                            \
    v->size = 0;                                                               \
    v->capacity = N;                                                           \
  }                                                                            \
                                                                               \
  void Prefix##_free(Alias *v) {                                               \
    if (v->heap) {                                                             \
      free(v->heap);                                                           \
    }                                                                          \
    v->heap = NULL;                                                            \
    v->size = 0;                                                               \
    v->capacity = N;                                                           \
  }                                                                            \
                                                                               \
  void Prefix##_push(Alias *v, T value) {                                      \
    if (v->size < N) {                                                         \
      v->stack[v->size++] = value;                                             \
    } else {                                                                   \
      if (v->size == v->capacity) {                                            \
        v->capacity = GROW_CAP(v->capacity);                                   \
        v->heap = realloc(v->heap, v->capacity * sizeof(T));                   \
      }                                                                        \
      v->heap[v->size++ - N] = value;                                          \
    }                                                                          \
  }                                                                            \
                                                                               \
  T *Prefix##_get(Alias *v, uint32_t idx) {                                    \
    if (idx < N) {                                                             \
      return &v->stack[idx];                                                   \
    } else {                                                                   \
      return &v->heap[idx - N];                                                \
    }                                                                          \
  }

DECLARE_SLICE(char, StringView, sv)

#endif // JCC_BASE_H
