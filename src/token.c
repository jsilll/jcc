#include "token.h"
#include "base.h"
#include "file.h"

DEFINE_ENUM_WITH_REPR(TokenKind, ENUMERATE_TOKENS)

DEFINE_VECTOR(Token, TokenStream, token_stream)

void token_stream_debug(FILE *out, const TokenStream *stream,
                        const SrcFile *file) {
  if (stream->size == 0) {
    return;
  }

  char fmt[32] = {0};
  const u32 digits = digit_count(file->num_lines);
  line_number_fmt(fmt, sizeof(fmt), digits);

  u32 line = 0;
  for (u32 i = 0; i < stream->size; ++i) {
    const Token token = stream->data[i];
    const Loc loc = src_file_get_loc(file, token.lex.data);
    if (loc.line != line) {
      fprintf(out, fmt, loc.line);
      line = loc.line;
    } else {
      fprintf(out, "%*s | ", digits, "");
    }
    switch (token.kind) {
    case TK_ID:
    case TK_INT:
    case TK_CHAR:
    case TK_STRING:
    case TK_COMMENT:
      fprintf(out, "%s: %.*s\n", TokenKind_Repr[token.kind], token.lex.size,
              token.lex.data);
      break;
    default:
      fprintf(out, "%s\n", TokenKind_Repr[token.kind]);
    }
  }
}
