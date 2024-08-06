#ifndef JCC_PARSE_H
#define JCC_PARSE_H

#include "ast.h"
#include "token.h"

typedef enum ParseErrorKind {
  PARSE_ERR_EXPECTED_EOF,
  PARSE_ERR_EXPECTED_SOME,
  PARSE_ERR_EXPECTED_TOKEN,
  PARSE_ERR_UNEXPECTED_EOF,
  PARSE_ERR_UNEXPECTED_TOKEN,
} ParseErrorKind;

typedef struct ParseError {
  ParseErrorKind kind;
  Token *token;
  TokenKind expected;
} ParseError;

DECLARE_VECTOR(ParseError, ParseErrorStream, parse_error_stream)

typedef struct ParseResult {
  FuncNode *ast;
  ParseErrorStream errors;
} ParseResult;

void parse_result_free(ParseResult *result);

ParseResult parse(Arena *arena, TokenStream *tokens);

#endif // JCC_PARSE_H
