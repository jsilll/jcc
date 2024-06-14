#ifndef JCC_SCAN_H
#define JCC_SCAN_H

#include "token.h"

typedef enum ScanErrorKind {
  SCAN_ERR_INVALID_CHAR,
  SCAN_ERR_INVALID_ESCAPE,
  SCAN_ERR_INVALID_SEQUENCE,
  SCAN_ERR_UNTERMINATED_CHAR,
  SCAN_ERR_UNTERMINATED_STRING,
} ScanErrorKind;

typedef struct ScanError {
  ScanErrorKind kind;
  StringView lex;
} ScanError;

DECLARE_VECTOR(ScanError, ScanErrorStream, scan_error_stream)

typedef struct ScanResult {
  TokenStream tokens;
  ScanErrorStream errors;
} ScanResult;

void scan_result_free(ScanResult *result);

ScanResult scan(SrcFile *file, bool comments);

#endif // JCC_SCAN_H
