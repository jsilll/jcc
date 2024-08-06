#ifndef JCC_BASE_H
#define JCC_BASE_H

#include <stdint.h>
#include <stdio.h>

#define TODO(msg)                                                              \
  do {                                                                         \
    fprintf(stderr, "[%s:%d] TODO: %s\n", __FILE__, __LINE__, msg);            \
    exit(EXIT_FAILURE);                                                        \
  } while (0)

#define PANIC(msg)                                                             \
  do {                                                                         \
    fprintf(stderr, "[%s:%d] PANIC: %s\n", __FILE__, __LINE__, msg);           \
    exit(EXIT_FAILURE);                                                        \
  } while (0)

#ifndef NDEBUG
#define DEBUG(msg)                                                             \
  fprintf(stderr, "[%s:%d] DEBUG: %s\n", __FILE__, __LINE__, msg)
#define DEBUGF(fmt, ...)                                                       \
  fprintf(stderr, "[%s:%d] DEBUG: " fmt "\n", __FILE__, __LINE__, __VA_ARGS__)
#else
#define DEBUG(msg)       // Do nothing in release mode
#define DEBUGF(fmt, ...) // Do nothing in release mode
#endif

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

#define GENERATE_ENUM(Enum) Enum,

#define GENERATE_STRING(String) #String,

#define DECLARE_REPR_ENUM(Alias, Enum)                                         \
  typedef enum Alias { Enum(GENERATE_ENUM) } Alias;                            \
  extern const char *const Alias##_Repr[];

#define DEFINE_REPR_ENUM(Alias, Enum)                                          \
  const char *const Alias##_Repr[] = {Enum(GENERATE_STRING)};

#define DECLARE_REPR_ENUM_MACRO(Alias, Enum, Macro)                            \
  typedef enum Alias { Enum(Macro) } Alias;                                    \
  extern const char *const Alias##_Repr[];

#define DEFINE_REPR_ENUM_MACRO(Alias, Enum, Macro)                             \
  const char *const Alias##_Repr[] = {Enum(Macro)};

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

#endif // JCC_BASE_H
