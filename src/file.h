#ifndef JCC_FILE_H
#define JCC_FILE_H

#include "base.h"

typedef enum FileResult {
  FILE_SUCCESS,
  FILE_ERR_OPEN,
  FILE_ERR_READ,
  FILE_ERR_EMPTY,
} FileResult;

typedef struct File {
  const char *name;
  char *data;
  char *end;
  char **lines;
  u32 num_lines;
} File;

typedef struct LineCol {
  u32 line;
  u32 col;
} LineCol;

FileResult file_init(File *file, const char *name);

void file_free(File *file);

void file_init_from_raw(File *file, const char *name, const char *data);

char *file_get_line(const File *file, const char *loc);

LineCol file_get_line_col(const File *file, const char *loc);

#endif // FILE_H
