#include "file.h"

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static FileResult file_read(SrcFile *file, const char *name) {
  FILE *fp = fopen(name, "r");
  if (fp == NULL) {
    return FILE_ERR_OPEN;
  }

  assert(fseek(fp, 0, SEEK_END) == 0);
  long size = ftell(fp);
  assert(fseek(fp, 0, SEEK_SET) == 0);

  assert(size >= 0);
  if (size == 0) {
    assert(fclose(fp) == 0);
    return FILE_ERR_EMPTY;
  }

  assert((file->data = malloc(size + 1)) != NULL);
  assert(fread(file->data, 1, size, fp) == (unsigned long)size);
  assert(fclose(fp) == 0);

  file->data[size] = '\0';
  file->end = file->data + size;
  return FILE_SUCCESS;
}

static void file_init_lines(SrcFile *file) {
  file->num_lines = 1;
  for (char *c = file->data; *c != '\0'; ++c) {
    if (*c == '\n') {
      ++file->num_lines;
    }
  }
  file->lines = malloc(file->num_lines * sizeof(char *));
  assert(file->lines != NULL);
  char **line = file->lines;
  *(line++) = file->data;
  for (char *c = file->data; *c != '\0'; ++c) {
    if (*c == '\n') {
      *c = '\0';
      *(line++) = c + 1;
    }
  }
}

FileResult src_file_init(SrcFile *file, const char *name) {
  const FileResult res = file_read(file, name);
  if (res != FILE_SUCCESS) {
    return res;
  }
  file->name = name;
  file_init_lines(file);
  return FILE_SUCCESS;
}

void src_file_free(SrcFile *file) {
  free(file->lines);
  free(file->data);
  file->data = NULL;
  file->end = NULL;
  file->lines = NULL;
  file->num_lines = 0;
}

void src_file_init_from_raw(SrcFile *file, const char *name, const char *data) {
  file->name = name;
  unsigned long size = strlen(data);
  file->data = malloc(size + 1);
  assert(file->data != NULL);
  file->data[size] = '\0';
  file->end = file->data + size;
  strcpy(file->data, data);
  file_init_lines(file);
}

Loc src_file_get_loc(const SrcFile *file, const char *loc) {
  uint32_t mid = 0;
  uint32_t left = 0;
  uint32_t right = file->num_lines - 1;
  while (left <= right) {
    mid = left + (right - left) / 2;
    char *line = file->lines[mid];
    if (loc < line) {
      right = mid - 1;
    } else if (loc > line) {
      left = mid + 1;
    } else {
      return (Loc){file->lines[mid], mid + 1, 1};
    }
  }
  if (loc < file->lines[mid]) {
    --mid;
  }
  return (Loc){file->lines[mid], mid + 1, loc - file->lines[mid] + 1};
}
