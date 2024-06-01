#ifndef SCAN_H
#define SCAN_H

#include "token.h"

typedef struct ScanError {
  enum {
    SCAN_ERR_INVALID_CHAR,
    SCAN_ERR_UNTERMINATED_STRING,
  } kind;
  StringView lex;
} ScanError;

DECLARE_VECTOR(ScanError, ScanErrorStream, scan_error_stream)

typedef struct ScanResult {
  TokenStream tokens;
  ScanErrorStream errors;
} ScanResult;

ScanResult scan(File *file);

void scan_result_free(ScanResult *result);

#endif // SCAN_H
