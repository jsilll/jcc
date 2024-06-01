#ifndef CC_MACROS_H
#define CC_MACROS_H

// -----------------------------------------------------------------------------
// Enum macros
// -----------------------------------------------------------------------------

#define GENERATE_ENUM(ENUM) ENUM,

#define GENERATE_STRING(STRING) #STRING,

// -----------------------------------------------------------------------------
// Debug macros
// -----------------------------------------------------------------------------

#define DEBUG(msg) fprintf(stderr, "%s:%d: " msg "\n", __FILE__, __LINE__)

#define DEBUGF(fmt, ...)                                                       \
  fprintf(stderr, "%s:%d: " fmt "\n", __FILE__, __LINE__, __VA_ARGS__)

#endif // CC_MACROS_H
