#ifndef JCC_SEMA_H
#define JCC_SEMA_H

#include "adt/vector.h"

#include "ast.h"

typedef enum SemaErrorKind { SEMA_ERR_MISMATCHED_TYPES } SemaErrorKind;

typedef struct SemaError {
  SemaErrorKind kind;
  StringView span;
} SemaError;

DECLARE_VEC(SemaError, SemaErrorVec, sema_error_vec)

typedef struct SemaResult {
  SemaErrorVec errors;
} SemaResult;

void sema_result_free(SemaResult *result);

SemaResult sema(Arena *arena, FuncNode *ast);

#endif // JCC_SEMA_H
