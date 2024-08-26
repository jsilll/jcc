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

DECLARE_VEC(ParseError, ParseErrorVec, parse_error_vec)

typedef struct ParseResult {
  FuncNode *ast;
  ParseErrorVec errors;
} ParseResult;

void parse_result_free(ParseResult *result);

ParseResult parse(Arena *arena, TokenVec *tokens);

#endif // JCC_PARSE_H
