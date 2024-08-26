#ifndef JCC_RESOLVE_H
#define JCC_RESOLVE_H

#include "adt/vector.h"

#include "ast.h"

typedef enum ResolveErrorKind {
  RESOLVE_ERR_DUPLICATE,
  RESOLVE_ERR_UNDECLARED,
} ResolveErrorKind;

typedef struct ResolveError {
  ResolveErrorKind kind;
  StringView span;
} ResolveError;

DECLARE_VEC(ResolveError, ResolveErrorVec, resolve_error_vec)

typedef struct ResolveResult {
  ResolveErrorVec errors;
} ResolveResult;

void resolve_result_free(ResolveResult *result);

ResolveResult resolve(FuncNode *ast);

#endif // JCC_RESOLVE_H
