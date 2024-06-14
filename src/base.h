#ifndef JCC_BASE_H
#define JCC_BASE_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

typedef uint8_t u8;
typedef int8_t i8;
typedef uint16_t u16;
typedef int16_t i16;
typedef uint32_t u32;
typedef int32_t i32;
typedef uint64_t u64;
typedef int64_t i64;
typedef float f32;
typedef double f64;

#define DEBUG(msg) fprintf(stderr, "[%s:%d] %s\n", __FILE__, __LINE__, msg)

#define DEBUGF(fmt, ...)                                                       \
  fprintf(stderr, "[%s:%d] " fmt "\n", __FILE__, __LINE__, __VA_ARGS__)

#define TODO(msg)                                                              \
  do {                                                                         \
    fprintf(stderr, "[%s:%d] TODO: %s\n", __FILE__, __LINE__, msg);            \
    exit(EXIT_FAILURE);                                                        \
  } while (0)

#define PANIC(msg)                                                             \
  do {                                                                         \
    fprintf(stderr, "[%s:%d] PANIC %s\n", __FILE__, __LINE__, msg);            \
    exit(EXIT_FAILURE);                                                        \
  } while (0)

#define GENERATE_ENUM(ENUM) ENUM,

#define GENERATE_STRING(STRING) #STRING,

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
    u32 size;                                                                  \
  } Alias;

#define DECLARE_VECTOR(T, Alias, Prefix)                                       \
  typedef struct Alias {                                                       \
    T *data;                                                                   \
    size_t size;                                                               \
    size_t capacity;                                                           \
  } Alias;                                                                     \
                                                                               \
  void Prefix##_init(Alias *v);                                                \
  void Prefix##_free(Alias *v);                                                \
  void Prefix##_push(Alias *v, T value);

#define DEFINE_VECTOR(T, Alias, Prefix)                                        \
  void Prefix##_init(Alias *v) {                                               \
    v->data = NULL;                                                            \
    v->size = 0;                                                               \
    v->capacity = 0;                                                           \
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
  }

#define DECLARE_SMALL_VECTOR(T, N, Alias, Prefix)                              \
  typedef struct Alias {                                                       \
    T stack[N];                                                                \
    T *heap;                                                                   \
    u32 size;                                                                  \
    u32 capacity;                                                              \
  } Alias;                                                                     \
                                                                               \
  void Prefix##_init(Alias *v);                                                \
  void Prefix##_free(Alias *v);                                                \
  void Prefix##_push(Alias *v, T value);                                       \
  T *Prefix##_get(Alias *v, u32 idx);

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
  T *Prefix##_get(Alias *v, u32 idx) {                                         \
    if (idx < N) {                                                             \
      return &v->stack[idx];                                                   \
    } else {                                                                   \
      return &v->heap[idx - N];                                                \
    }                                                                          \
  }

DECLARE_SLICE(char, StringView, sv)

static inline u32 digit_count(u32 n) {
  u32 digits = 1;
  for (u32 i = n; i >= 10; i /= 10) {
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

static inline void line_number_fmt(char *s, size_t n, u32 digits) {
  snprintf(s, n, "%%%dd | ", digits);
}

#endif // JCC_BASE_H
