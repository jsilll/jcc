#include "support/error.h"

#include "support/base.h"

#include <stdarg.h>

void error(const char *msg, ...) {
  va_list args;
  va_start(args, msg);
  fprintf(stderr, "error: ");
  vfprintf(stderr, msg, args);
  fprintf(stderr, "\n");
  va_end(args);
}

// NOLINTNEXTLINE
static void verror_at(const SrcFile *file, StringView span, const char *title,
                      const char *msg, va_list args) {
  char fmt[32] = {0};
  const Loc loc = sf_loc(file, span.data);
  const uint32_t digits = digit_count(loc.line);
  line_number_fmt(fmt, sizeof(fmt), digits);

  fprintf(stderr, "error: %s\n", title);
  fprintf(stderr, "%*s--> %s:%d:%d\n", digits, "", file->name, loc.line,
          loc.col);
  fprintf(stderr, "%*s |\n", digits, "");
  fprintf(stderr, fmt, loc.line);
  fprintf(stderr, "%s\n", loc.str);

  if (span.size <= 1) {
    fprintf(stderr, "%*s | %*s^\n", digits, "", loc.col - 1, "");
  } else {
    fprintf(stderr, "%*s | %*s^", digits, "", loc.col - 1, "");
    for (uint32_t i = 0; i < span.size - 1; ++i) {
      fprintf(stderr, "~");
    }
    fprintf(stderr, " ");
    vfprintf(stderr, msg, args);
    fprintf(stderr, "\n");
  }
}

void error_at(const SrcFile *file, StringView span, const char *title,
              const char *msg, ...) {
  va_list args;
  va_start(args, msg);
  verror_at(file, span, title, msg, args);
  va_end(args);
}
