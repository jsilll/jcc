#include "error.h"
#include "file.h"
#include "scan.h"

#include <string.h>

static void print_usage(const char *name) {
  fprintf(stderr, "usage: %s [ <stmt> | --file <file> ]\n", name);
}

static void report_file_errors(const char *name, FileResult result) {
  switch (result) {
  case FILE_SUCCESS:
    PANIC("unexpected file result");
    break;
  case FILE_ERR_EMPTY:
    fprintf(stderr, "error: file '%s' not found\n", name);
    break;
  case FILE_ERR_OPEN:
    fprintf(stderr, "error: failed to open file '%s'\n", name);
    break;
  case FILE_ERR_READ:
    fprintf(stderr, "error: failed to read file '%s'\n", name);
    break;
  }
}

static void report_scan_errors(const File *file, const ScanErrorStream *errors,
                               u32 max) {
  max = MIN(max, errors->size);
  for (u32 i = 0; i < max; ++i) {
    switch (errors->data[i].kind) {
    case SCAN_ERR_INVALID_CHAR:
      if (IS_ASCII(*errors->data[i].lex.data)) {
        error_at(file, errors->data[i].lex, "unexpected character",
                 "character '%c' is not allowed", *errors->data[i].lex.data);
      } else {
        error_at(file, errors->data[i].lex, "unexpected character",
                 "character is not an ASCII");
      }
      break;
    case SCAN_ERR_INVALID_SEQUENCE:
      error_at(file, errors->data[i].lex, "invalid sequence",
               "this sequence of characters does not match any token");
      break;
    case SCAN_ERR_UNTERMINATED_STRING:
      error_at(file, errors->data[i].lex, "unterminated string",
               "string is not terminated");
      break;
    }
  }
}

int main(int argc, char *argv[]) {
  if (argc == 1 || argc > 3) {
    print_usage(argv[0]);
    return EXIT_FAILURE;
  }

  // Open file
  File file;
  if (argc == 2) {
    file_init_from_raw(&file, "stdin", argv[1]);
  } else if (argc == 3 && strcmp(argv[1], "--file") == 0) {
    FileResult fr = file_init(&file, argv[2]);
    if (fr != FILE_SUCCESS) {
      report_file_errors(argv[2], fr);
      return EXIT_FAILURE;
    }
  } else {
    print_usage(argv[0]);
    return EXIT_FAILURE;
  }

  // Scan file
  ScanResult sr = scan(&file, true);
  if (sr.errors.size > 0) {
    report_scan_errors(&file, &sr.errors, 1);
    scan_result_free(&sr);
    file_free(&file);
    return EXIT_FAILURE;
  }

  // TODO: Only for debugging
  token_stream_debug(&sr.tokens, &file);

  // TODO: Parse tokens

  scan_result_free(&sr);
  file_free(&file);
  return EXIT_SUCCESS;
}
