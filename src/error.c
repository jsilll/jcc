#include "error.h"

#include <stdarg.h>

void error(const char *msg, ...) {
  va_list args;
  va_start(args, msg);
  fprintf(stderr, "error: ");
  vfprintf(stderr, msg, args);
  fprintf(stderr, "\n");
  va_end(args);
}

static inline u32 count_digits(u32 n) {
  u32 digits = 1;
  for (u32 i = n; i >= 10; i /= 10) {
    ++digits;
  }
  return digits;
}

static inline char *line_number_fmt(u32 line_digits) {
  static char fmt[16];
  sprintf(fmt, " %%%dd | %%s\n", line_digits);
  return fmt;
}

static void verror_at(const File *file, StringView sv, const char *title,
                      const char *msg, va_list args) {
  const char *line = file_get_line(file, sv.data);
  const LineCol lc = file_get_line_col(file, sv.data);
  const u32 line_digits = count_digits(lc.line);
  const char *const fmt = line_number_fmt(line_digits);
  fprintf(stderr, "error: %s\n", title);
  fprintf(stderr, " %*s--> %s:%d:%d\n", line_digits, "", file->name, lc.line,
          lc.col);
  fprintf(stderr, " %*s |\n", line_digits, "");
  fprintf(stderr, fmt, lc.line, line);
  if (sv.size <= 1) {
    fprintf(stderr, " %*s | %*s┌\n", line_digits, "", lc.col - 1, "");
  } else {
    fprintf(stderr, " %*s | %*s┌", line_digits, "", lc.col - 1, "");
    for (uint32_t i = 0; i < sv.size - 1; ++i) {
      fprintf(stderr, "─");
    }
    fprintf(stderr, "\n");
  }
  fprintf(stderr, " %*s | %*s└─ ", line_digits, "", lc.col - 1, "");
  vfprintf(stderr, msg, args);
  fprintf(stderr, "\n");
}

void error_at(const File *file, StringView sv, const char *title,
              const char *msg, ...) {
  va_list args;
  va_start(args, msg);
  verror_at(file, sv, title, msg, args);
  va_end(args);
}
