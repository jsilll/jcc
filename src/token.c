#include "token.h"

DEFINE_REPR_ENUM(TokenKind, ENUMERATE_TOKENS)

const char *token_kind_lex(TokenKind kind) {
  switch (kind) {
  case TK_LBRACK:
    return "'['";
  case TK_RBRACK:
    return "']'";
  case TK_LPAREN:
    return "'('";
  case TK_RPAREN:
    return "')'";
  case TK_LBRACE:
    return "'{'";
  case TK_RBRACE:
    return "'}'";
  case TK_DOT:
    return "'.'";
  case TK_ARROW:
    return "'->'";
  case TK_PLUS_PLUS:
    return "'++'";
  case TK_MINUS_MINUS:
    return "'--'";
  case TK_AMP:
    return "'&'";
  case TK_STAR:
    return "'*'";
  case TK_PLUS:
    return "'+'";
  case TK_MINUS:
    return "'-'";
  case TK_TILDE:
    return "'~'";
  case TK_BANG:
    return "'!'";
  case TK_SLASH:
    return "'/'";
  case TK_PERCENT:
    return "'%'";
  case TK_LT_LT:
    return "'<<'";
  case TK_GT_GT:
    return "'>>'";
  case TK_LT:
    return "'<'";
  case TK_GT:
    return "'>'";
  case TK_LT_EQ:
    return "'<='";
  case TK_GT_EQ:
    return "'>='";
  case TK_EQ_EQ:
    return "'=='";
  case TK_BANG_EQ:
    return "'!='";
  case TK_CARET:
    return "'^'";
  case TK_PIPE:
    return "'|'";
  case TK_AMP_AMP:
    return "'&&'";
  case TK_PIPE_PIPE:
    return "'||'";
  case TK_QUESTION:
    return "'?'";
  case TK_COLON:
    return "':'";
  case TK_SEMICOLON:
    return "';'";
  case TK_DOT_DOT_DOT:
    return "'...'";
  case TK_EQ:
    return "'='";
  case TK_STAR_EQ:
    return "'*='";
  case TK_SLASH_EQ:
    return "'/='";
  case TK_PERCENT_EQ:
    return "'%='";
  case TK_PLUS_EQ:
    return "'+='";
  case TK_MINUS_EQ:
    return "'-='";
  case TK_LT_LT_EQ:
    return "'<<='";
  case TK_GT_GT_EQ:
    return "'>>='";
  case TK_AMP_EQ:
    return "'&='";
  case TK_CARET_EQ:
    return "'^='";
  case TK_PIPE_EQ:
    return "'|='";
  case TK_COMMA:
    return "','";
  case TK_HASH:
    return "'#'";
  case TK_HASH_HASH:
    return "'##'";
  case TK_LT_COLON:
    return "'<:'";
  case TK_COLON_GT:
    return "':>'";
  case TK_LT_PERCENT:
    return "'%'";
  case TK_PERCENT_GT:
    return "'>'";
  case TK_PERCENT_COLON:
    return "'%:'";
  case TK_PERCENT_COLON_PERCENT_COLON:
    return "'%:%:'";
  case TK_KW_ALIGNOF:
    return "'alignof'";
  case TK_KW_AUTO:
    return "'auto'";
  case TK_KW_BREAK:
    return "'break'";
  case TK_KW_CASE:
    return "'case'";
  case TK_KW_CHAR:
    return "'char'";
  case TK_KW_CONST:
    return "'const'";
  case TK_KW_CONTINUE:
    return "'continue'";
  case TK_KW_DEFAULT:
    return "'default'";
  case TK_KW_DO:
    return "'do'";
  case TK_KW_DOUBLE:
    return "'double'";
  case TK_KW_ELSE:
    return "'else'";
  case TK_KW_ENUM:
    return "'enum'";
  case TK_KW_EXTERN:
    return "'extern'";
  case TK_KW_FLOAT:
    return "'float'";
  case TK_KW_FOR:
    return "'for'";
  case TK_KW_GOTO:
    return "'goto'";
  case TK_KW_IF:
    return "'if'";
  case TK_KW_INLINE:
    return "'inline'";
  case TK_KW_INT:
    return "'int'";
  case TK_KW_LONG:
    return "'long'";
  case TK_KW_REGISTER:
    return "'register'";
  case TK_KW_RESTRICT:
    return "'restrict'";
  case TK_KW_RETURN:
    return "'return'";
  case TK_KW_SHORT:
    return "'short'";
  case TK_KW_SIGNED:
    return "'signed'";
  case TK_KW_SIZEOF:
    return "'sizeof'";
  case TK_KW_STATIC:
    return "'static'";
  case TK_KW_STRUCT:
    return "'struct'";
  case TK_KW_SWITCH:
    return "'switch'";
  case TK_KW_TYPEDEF:
    return "'typedef'";
  case TK_KW_UNION:
    return "'union'";
  case TK_KW_UNSIGNED:
    return "'unsigned'";
  case TK_KW_VOID:
    return "'void'";
  case TK_KW_VOLATILE:
    return "'volatile'";
  case TK_KW_WHILE:
    return "'while'";
  case TK_KW__ALIGNAS:
    return "'_Alignas'";
  case TK_KW__ATOMIC:
    return "'_Atomic'";
  case TK_KW__BOOL:
    return "'_Bool'";
  case TK_KW__COMPLEX:
    return "'_Complex'";
  case TK_KW__GENERIC:
    return "'_Generic'";
  case TK_KW__IMAGINARY:
    return "'_Imaginary'";
  case TK_KW__NORETURN:
    return "'_Noreturn'";
  case TK_KW__STATIC_ASSERT:
    return "'_Static_assert'";
  case TK_KW__THREAD_LOCAL:
    return "'_Thread_local'";
  case TK_NUM:
    return "an integer literal";
  case TK_CHAR:
    return "a character literal";
  case TK_IDENT:
    return "an identifier";
  case TK_FLOAT:
    return "a float literal";
  case TK_STRING:
    return "a string literal";
  case TK_COMMENT:
    return "a comment";
  }
  return "unknown";
}

DEFINE_VECTOR(Token, TokenStream, token_stream)

void token_stream_debug(FILE *out, const TokenStream *stream,
                        const SrcFile *file) {
  if (stream->size == 0) {
    return;
  }

  char fmt[32] = {0};
  const uint32_t digits = digit_count(file->num_lines);
  line_number_fmt(fmt, sizeof(fmt), digits);

  uint32_t line = 0;
  for (uint32_t i = 0; i < stream->size; ++i) {
    const Token token = stream->data[i];
    const Loc loc = src_file_get_loc(file, token.lex.data);
    if (loc.line != line) {
      fprintf(out, fmt, loc.line);
      line = loc.line;
    } else {
      fprintf(out, "%*s | ", digits, "");
    }
    switch (token.kind) {
    case TK_NUM:
    case TK_CHAR:
    case TK_IDENT:
    case TK_STRING:
    case TK_COMMENT:
      fprintf(out, "%s: %.*s\n", TokenKind_Repr[token.kind],
              (int)token.lex.size, token.lex.data);
      break;
    default:
      fprintf(out, "%s\n", TokenKind_Repr[token.kind]);
    }
  }
}
