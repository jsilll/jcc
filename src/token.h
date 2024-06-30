#ifndef JCC_TOKEN_H
#define JCC_TOKEN_H

#include "base.h"
#include "file.h"

#define ENUMERATE_TOKENS(M)                                                    \
  M(TK_NUM)                                                                    \
  M(TK_CHAR)                                                                   \
  M(TK_IDENT)                                                                  \
  M(TK_FLOAT)                                                                  \
  M(TK_STRING)                                                                 \
                                                                               \
  M(TK_LBRACK)                                                                 \
  M(TK_RBRACK)                                                                 \
  M(TK_LPAREN)                                                                 \
  M(TK_RPAREN)                                                                 \
  M(TK_LBRACE)                                                                 \
  M(TK_RBRACE)                                                                 \
  M(TK_DOT)                                                                    \
  M(TK_ARROW)                                                                  \
  M(TK_PLUS_PLUS)                                                              \
  M(TK_MINUS_MINUS)                                                            \
  M(TK_AMP)                                                                    \
  M(TK_STAR)                                                                   \
  M(TK_PLUS)                                                                   \
  M(TK_MINUS)                                                                  \
  M(TK_TILDE)                                                                  \
  M(TK_BANG)                                                                   \
  M(TK_SLASH)                                                                  \
  M(TK_PERCENT)                                                                \
  M(TK_LT_LT)                                                                  \
  M(TK_GT_GT)                                                                  \
  M(TK_LT)                                                                     \
  M(TK_GT)                                                                     \
  M(TK_LT_EQ)                                                                  \
  M(TK_GT_EQ)                                                                  \
  M(TK_EQ_EQ)                                                                  \
  M(TK_BANG_EQ)                                                                \
  M(TK_CARET)                                                                  \
  M(TK_PIPE)                                                                   \
  M(TK_AMP_AMP)                                                                \
  M(TK_PIPE_PIPE)                                                              \
  M(TK_QUESTION)                                                               \
  M(TK_COLON)                                                                  \
  M(TK_SEMICOLON)                                                              \
  M(TK_DOT_DOT_DOT)                                                            \
  M(TK_EQ)                                                                     \
  M(TK_STAR_EQ)                                                                \
  M(TK_SLASH_EQ)                                                               \
  M(TK_PERCENT_EQ)                                                             \
  M(TK_PLUS_EQ)                                                                \
  M(TK_MINUS_EQ)                                                               \
  M(TK_LT_LT_EQ)                                                               \
  M(TK_GT_GT_EQ)                                                               \
  M(TK_AMP_EQ)                                                                 \
  M(TK_CARET_EQ)                                                               \
  M(TK_PIPE_EQ)                                                                \
  M(TK_COMMA)                                                                  \
  M(TK_HASH)                                                                   \
  M(TK_HASH_HASH)                                                              \
  M(TK_LT_COLON)                                                               \
  M(TK_COLON_GT)                                                               \
  M(TK_LT_PERCENT)                                                             \
  M(TK_PERCENT_GT)                                                             \
  M(TK_PERCENT_COLON)                                                          \
  M(TK_PERCENT_COLON_PERCENT_COLON)                                            \
                                                                               \
  M(TK_KW_ALIGNOF)                                                             \
  M(TK_KW_AUTO)                                                                \
  M(TK_KW_BREAK)                                                               \
  M(TK_KW_CASE)                                                                \
  M(TK_KW_CHAR)                                                                \
  M(TK_KW_CONST)                                                               \
  M(TK_KW_CONTINUE)                                                            \
  M(TK_KW_DEFAULT)                                                             \
  M(TK_KW_DO)                                                                  \
  M(TK_KW_DOUBLE)                                                              \
  M(TK_KW_ELSE)                                                                \
  M(TK_KW_ENUM)                                                                \
  M(TK_KW_EXTERN)                                                              \
  M(TK_KW_FLOAT)                                                               \
  M(TK_KW_FOR)                                                                 \
  M(TK_KW_GOTO)                                                                \
  M(TK_KW_IF)                                                                  \
  M(TK_KW_INLINE)                                                              \
  M(TK_KW_INT)                                                                 \
  M(TK_KW_LONG)                                                                \
  M(TK_KW_REGISTER)                                                            \
  M(TK_KW_RESTRICT)                                                            \
  M(TK_KW_RETURN)                                                              \
  M(TK_KW_SHORT)                                                               \
  M(TK_KW_SIGNED)                                                              \
  M(TK_KW_SIZEOF)                                                              \
  M(TK_KW_STATIC)                                                              \
  M(TK_KW_STRUCT)                                                              \
  M(TK_KW_SWITCH)                                                              \
  M(TK_KW_TYPEDEF)                                                             \
  M(TK_KW_UNION)                                                               \
  M(TK_KW_UNSIGNED)                                                            \
  M(TK_KW_VOID)                                                                \
  M(TK_KW_VOLATILE)                                                            \
  M(TK_KW_WHILE)                                                               \
  M(TK_KW__ALIGNAS)                                                            \
  M(TK_KW__ATOMIC)                                                             \
  M(TK_KW__BOOL)                                                               \
  M(TK_KW__COMPLEX)                                                            \
  M(TK_KW__GENERIC)                                                            \
  M(TK_KW__IMAGINARY)                                                          \
  M(TK_KW__NORETURN)                                                           \
  M(TK_KW__STATIC_ASSERT)                                                      \
  M(TK_KW__THREAD_LOCAL)                                                       \
                                                                               \
  M(TK_COMMENT)

DECLARE_REPR_ENUM(TokenKind, ENUMERATE_TOKENS)

const char *token_kind_lex(TokenKind kind);

typedef struct Token {
  TokenKind kind;
  StringView lex;
} Token;

DECLARE_VECTOR(Token, TokenStream, token_stream)

void token_stream_debug(FILE *out, const TokenStream *stream,
                        const SrcFile *file);

#endif // JCC_TOKEN_H
