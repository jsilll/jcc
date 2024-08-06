#ifndef JCC_ERROR_H
#define JCC_ERROR_H

#include "adt/slice.h"

#include "support/file.h"

void error(const char *msg, ...);
void error_at(const SrcFile *file, StringView sv, const char *title,
              const char *msg, ...);

#endif // JCC_ERROR_H
