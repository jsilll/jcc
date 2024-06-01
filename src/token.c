#include "token.h"
#include "file.h"

const char *const TOKEN_KIND_STR[] = {ENUMERATE_TOKENS(GENERATE_STRING)};

DEFINE_VECTOR(Token, TokenStream, token_stream)

void token_stream_debug(const TokenStream *stream, File *file) {
  u32 line = 0;
  for (u32 i = 0; i < stream->size; ++i) {
    Token token = stream->data[i];
    LineCol lc = file_get_line_col(file, token.lex.data);
    if (lc.line != line) {
      printf("%7d | ", lc.line);
      line = lc.line;
    } else {
      printf("        | ");
    }
    switch (token.kind) {
    case TK_ID:
      printf("%s: %.*s\n", TOKEN_KIND_STR[token.kind], token.lex.size,
             token.lex.data);
      break;
    case TK_INT:
      printf("%s: %.*s\n", TOKEN_KIND_STR[token.kind], token.lex.size,
             token.lex.data);
      break;
    case TK_STRING:
      printf("%s: %.*s\n", TOKEN_KIND_STR[token.kind], token.lex.size,
             token.lex.data);
      break;
    default:
      TODO("Implement token_stream_debug for other token kinds");
    }
  }
}
