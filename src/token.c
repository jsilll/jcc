#include "token.h"
#include "file.h"

const char *const TOKEN_KIND_STR[] = {ENUMERATE_TOKENS(GENERATE_STRING)};

DEFINE_VECTOR(Token, TokenStream, token_stream)

static inline u32 count_digits(u32 n) {
  u32 digits = 1;
  for (u32 i = n; i >= 10; i /= 10) {
    ++digits;
  }
  return digits;
}

static inline char *line_number_fmt(u32 line_digits) {
  static char fmt[16];
  sprintf(fmt, "%%%dd | ", line_digits);
  return fmt;
}

void token_stream_debug(const TokenStream *stream, File *file) {
  const u32 last_line =
      file_get_line_col(file, stream->data[stream->size - 1].lex.data).line;
  const u32 line_digits = count_digits(last_line);
  const char *const fmt = line_number_fmt(line_digits);
  u32 line = 0;
  for (u32 i = 0; i < stream->size; ++i) {
    const Token token = stream->data[i];
    const LineCol lc = file_get_line_col(file, token.lex.data);
    if (lc.line != line) {
      printf(fmt, lc.line);
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
      printf("%s\n", TOKEN_KIND_STR[token.kind]);
    }
  }
}
