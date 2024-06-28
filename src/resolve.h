#ifndef JCC_RESOLVE_H
#define JCC_RESOLVE_H

#include "ast.h"

typedef enum ResolveErrorKind {
  RESOLVE_ERR_DUPLICATE,
  RESOLVE_ERR_UNDECLARED,
} ResolveErrorKind;

typedef struct ResolveError {
  ResolveErrorKind kind;
  StringView span;
} ResolveError;

DECLARE_VECTOR(ResolveError, ResolveErrorStream, resolve_error_stream)

typedef struct ResolveResult {
  ResolveErrorStream errors;
} ResolveResult;

void resolve_result_free(ResolveResult *result);

ResolveResult resolve(FuncNode *ast);

#endif // JCC_RESOLVE_H
