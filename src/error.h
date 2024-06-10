#ifndef JCC_ERROR_H
#define JCC_ERROR_H

#include "file.h"

void error(const char *msg, ...);

void error_at(const File *file, StringView sv, const char *title,
              const char *msg, ...);

#endif // JCC_ERROR_H
