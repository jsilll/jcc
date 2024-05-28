#ifndef CC_LEXER_H
#define CC_LEXER_H

#include "string_view.h"
#include "utility.h"

#define ENUMERATE_TOKENS(M)                                                    \
  M(TK_ERR)                                                                    \
  M(TK_EOF)                                                                    \
  M(TK_SEMI)                                                                   \
  M(TK_LPAREN)                                                                 \
  M(TK_RPAREN)                                                                 \
  M(TK_LBRACE)                                                                 \
  M(TK_RBRACE)                                                                 \
  M(TK_PLUS)                                                                   \
  M(TK_MINUS)                                                                  \
  M(TK_MUL)                                                                    \
  M(TK_DIV)                                                                    \
  M(TK_EQ)                                                                     \
  M(TK_EQEQ)                                                                   \
  M(TK_NE)                                                                     \
  M(TK_LT)                                                                     \
  M(TK_LE)                                                                     \
  M(TK_GT)                                                                     \
  M(TK_GE)                                                                     \
  M(TK_NUM)                                                                    \
  M(TK_RETURN)                                                                 \
  M(TK_IDENT)

/// The kind of a token.
typedef enum { ENUMERATE_TOKENS(GENERATE_ENUM) } TokenKind;

/// The string representation of a token kind.
static const char *TOKEN_KIND_STR[] = {ENUMERATE_TOKENS(GENERATE_STRING)};

/// A token.
typedef struct {
  StringView lex;
  TokenKind kind;
  union {
    float f;
    int32_t i;
  } val;
} Token;

/// A lexer.
typedef struct Lexer {
  bool consumed;
  char *ptr;
  Token token;
} Lexer;

/// Create a lexer.
/// @param ptr The input string.
Lexer lexer_create(char *ptr);

/// Peek at the next token.
/// @param lexer The lexer.
Token *lexer_peek(Lexer *lexer);

/// Get the next token.
/// @param lexer The lexer.
Token *lexer_next(Lexer *lexer);

#endif // CC_LEXER_H
