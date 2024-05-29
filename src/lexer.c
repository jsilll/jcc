#include "lexer.h"

#include <ctype.h>
#include <stdlib.h>
#include <string.h>

static Token tokenize_single_char(char *p, TokenKind k) {
  return (Token){.kind = k, .lex = {p, 1}};
}

static Token tokenize_double_char(char *p, char c, TokenKind k1, TokenKind k2) {
  if (p[1] == c) {
    return (Token){.kind = k1, .lex = {p, 2}};
  } else {
    return (Token){.kind = k2, .lex = {p, 1}};
  }
}

static Token tokenize_word(char *p) {
  Token t = {0};
  t.lex.ptr = p;
  while (isalnum(*p) || *p == '_') {
    ++p;
  }
  t.lex.len = p - t.lex.ptr;
  if (t.lex.len == 6 && strncmp(t.lex.ptr, "return", 6) == 0) {
    t.kind = TK_RETURN;
  } else {
    t.kind = TK_IDENT;
  }
  return t;
}

static Token tokenize_numeric_literal(char *p) {
  Token t = {0};
  t.kind = TK_NUM;
  t.lex.ptr = p;
  t.value.i = strtol(p, &p, 10);
  t.lex.len = p - t.lex.ptr;
  return t;
}

static Token tokenize(char *p) {
  while (*p) {
    // Skip whitespace
    if (isspace(*p)) {
      ++p;
      continue;
    }

    // Identifier and keywords
    if (isalpha(*p) || *p == '_') {
      return tokenize_word(p);
    }

    // Numeric literal
    if (isdigit(*p)) {
      return tokenize_numeric_literal(p);
    }

    // Punctuation
    switch (*p) {
    case ';':
      return tokenize_single_char(p, TK_SEMI);
    case '(':
      return tokenize_single_char(p, TK_LPAREN);
    case ')':
      return tokenize_single_char(p, TK_RPAREN);
    case '{':
      return tokenize_single_char(p, TK_LBRACE);
    case '}':
      return tokenize_single_char(p, TK_RBRACE);
    case '+':
      return tokenize_single_char(p, TK_PLUS);
    case '-':
      return tokenize_single_char(p, TK_MINUS);
    case '*':
      return tokenize_single_char(p, TK_MUL);
    case '/':
      return tokenize_single_char(p, TK_DIV);
    case '<':
      return tokenize_double_char(p, '=', TK_LE, TK_LT);
    case '>':
      return tokenize_double_char(p, '=', TK_GE, TK_GT);
    case '!':
      return tokenize_double_char(p, '=', TK_NE, TK_ERR);
    case '=':
      return tokenize_double_char(p, '=', TK_EQEQ, TK_EQ);
    default:
      return tokenize_single_char(p, TK_ERR);
    }
  }

  return tokenize_single_char(p, TK_EOF);
}

Lexer lexer_create(char *ptr) {
  Lexer l = {0};
  l.consumed = true;
  l.ptr = ptr;
  return l;
}

Token *lexer_peek(Lexer *l) {
  if (l->token.kind == TK_EOF) {
    return &l->token;
  }
  if (l->consumed) {
    l->consumed = false;
    l->token = tokenize(l->ptr);
    l->ptr = l->token.lex.ptr + l->token.lex.len;
  }
  return &l->token;
}

Token *lexer_next(Lexer *l) {
  if (l->token.kind == TK_EOF) {
    return &l->token;
  }
  if (l->consumed) {
    l->token = tokenize(l->ptr);
    l->ptr = l->token.lex.ptr + l->token.lex.len;
  }
  l->consumed = true;
  return &l->token;
}
