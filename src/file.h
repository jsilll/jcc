#ifndef JCC_FILE_H
#define JCC_FILE_H

#include <stdint.h>

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
  uint32_t num_lines;
} SrcFile;

typedef struct Loc {
  const char *str;
  uint32_t line;
  uint32_t col;
} Loc;

FileResult src_file_init(SrcFile *file, const char *name);
void src_file_free(SrcFile *file);
void src_file_init_from_raw(SrcFile *file, const char *name, const char *data);
uint32_t src_file_get_num_lines(const SrcFile *file);
Loc src_file_get_loc(const SrcFile *file, const char *loc);

#endif // JCC_FILE_H
