#ifndef JCC_SCAN_H
#define JCC_SCAN_H

#include "token.h"

typedef struct ScanError {
  enum {
    SCAN_ERR_INVALID_CHAR,
    SCAN_ERR_INVALID_SEQUENCE,
    SCAN_ERR_UNTERMINATED_STRING,
  } kind;
  StringView lex;
} ScanError;

DECLARE_VECTOR(ScanError, ScanErrorStream, scan_error_stream)

typedef struct ScanResult {
  TokenStream tokens;
  ScanErrorStream errors;
} ScanResult;

ScanResult scan(File *file, bool comments);

void scan_result_free(ScanResult *result);

#endif // JCC_SCAN_H
