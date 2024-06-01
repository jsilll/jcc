#ifndef ERROR_H
#define ERROR_H

#include "file.h"

void error(const char *msg, ...);

void error_at(const File *file, StringView sv, const char *title,
              const char *msg, ...);

#endif // ERROR_H
