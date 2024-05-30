#ifndef CC_LEXER_H
#define CC_LEXER_H

#include "macros.h"
#include "string_view.h"

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
  M(TK_IDENT)                                                                  \
  M(TK_KW_IF)                                                                  \
  M(TK_KW_ELSE)                                                                \
  M(TK_KW_FOR)                                                                 \
  M(TK_KW_RETURN)                                                              \
  M(TK_KW_WHILE)

/// The kind of a token.
typedef enum { ENUMERATE_TOKENS(GENERATE_ENUM) } TokenKind;

/// The string representation of a token kind.
static const char *const TOKEN_KIND_STR[] = {ENUMERATE_TOKENS(GENERATE_STRING)};

/// An error message for an unexpected token kind.
#define ERROR_TOKEN_KIND(token)                                                \
  error("unexpected token kind during %s:\n-> kind: %s\n-> lex: '%.*s'\n",     \
        __FUNCTION__, TOKEN_KIND_STR[(token)->kind], (int)(token)->lex.len,    \
        (token)->lex.ptr)

/// A token.
typedef struct {
  StringView lex;
  TokenKind kind;
  union {
    int32_t num;
  } u;
} Token;

/// A lexer.
typedef struct {
  Token token;
  bool consumed;
  const char *ptr;
} Lexer;

/// Create a lexer.
/// @param ptr The input string.
Lexer lexer_create(const char *ptr);

/// Peek at the next token.
/// @param lexer The lexer.
Token *lexer_peek(Lexer *lexer);

/// Get the next token.
/// @param lexer The lexer.
Token *lexer_next(Lexer *lexer);

#endif // CC_LEXER_H
