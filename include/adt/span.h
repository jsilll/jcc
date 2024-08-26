#ifndef JCC_SPAN_H
#define JCC_SPAN_H

#include <stdbool.h>

#define DECLARE_SPAN(T, Alias, Prefix)                                         \
  typedef struct Alias {                                                       \
    T *data;                                                                   \
    size_t size;                                                               \
  } Alias;

#endif // JCC_SPAN_H
