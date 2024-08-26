#ifndef JCC_SCAN_H
#define JCC_SCAN_H

#include "adt/vector.h"

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

DECLARE_VEC(ScanError, ScanErrorVec, scan_error_vec)

typedef struct ScanResult {
  TokenVec tokens;
  ScanErrorVec errors;
} ScanResult;

void scan_result_free(ScanResult *result);

ScanResult scan(const SrcFile *file, bool comments);

#endif // JCC_SCAN_H
