#ifndef BASE_H
#define BASE_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

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

typedef struct Allocator {
  void *ctx;
  void *(*alloc)(void *ctx, size_t size);
} Allocator;

#define DECLARE_SLICE(T, V, P)                                                 \
  typedef struct V {                                                           \
    T *data;                                                                   \
    u32 size;                                                                  \
  } V;

#define DECLARE_VECTOR(T, V, P)                                                \
  typedef struct V {                                                           \
    T *data;                                                                   \
    size_t size;                                                               \
    size_t capacity;                                                           \
  } V;                                                                         \
                                                                               \
  void P##_init(V *v);                                                         \
  void P##_free(V *v);                                                         \
  void P##_push(V *v, T value);

#define DEFINE_VECTOR(T, V, P)                                                 \
  void P##_init(V *v) {                                                        \
    v->data = NULL;                                                            \
    v->size = 0;                                                               \
    v->capacity = 0;                                                           \
  }                                                                            \
                                                                               \
  void P##_free(V *v) {                                                        \
    free(v->data);                                                             \
    v->data = NULL;                                                            \
    v->size = 0;                                                               \
    v->capacity = 0;                                                           \
  }                                                                            \
                                                                               \
  void P##_push(V *v, T value) {                                               \
    if (v->size == v->capacity) {                                              \
      v->capacity = GROW_CAP(v->capacity);                                     \
      v->data = realloc(v->data, v->capacity * sizeof(T));                     \
    }                                                                          \
    v->data[v->size++] = value;                                                \
  }

DECLARE_SLICE(char, StringView, sv)

#endif // BASE_H
