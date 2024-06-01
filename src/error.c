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

static void verror_at(const File *file, StringView sv, const char *title,
                      const char *msg, va_list args) {
  char *line = file_get_line(file, sv.data);
  LineCol lc = file_get_line_col(file, sv.data);
  fprintf(stderr, "error: %s\n", title);
  fprintf(stderr, "      --> %s:%d:%d\n", file->name, lc.line, lc.col);
  fprintf(stderr, "        |\n");
  fprintf(stderr, "%7d | %s\n", lc.line, line);
  if (sv.size <= 1) {
    fprintf(stderr, "        | %*s┌\n", lc.col - 1, "");
  } else {
    fprintf(stderr, "        | %*s┌", lc.col - 1, "");
    for (uint32_t i = 0; i < sv.size - 1; ++i) {
      fprintf(stderr, "─");
    }
    fprintf(stderr, "\n");
  }
  fprintf(stderr, "        | %*s└─ ", lc.col - 1, "");
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
