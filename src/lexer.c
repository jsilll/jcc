#include "lexer.h"

#include <ctype.h>
#include <stdlib.h>
#include <string.h>

static Token tokenize_single_char(const char *p, TokenKind k) {
  return (Token){.kind = k, .lex = {p, 1}};
}

static Token tokenize_double_char(const char *p, char c, TokenKind k1,
                                  TokenKind k2) {
  if (p[1] == c) {
    return (Token){.kind = k1, .lex = {p, 2}};
  } else {
    return (Token){.kind = k2, .lex = {p, 1}};
  }
}

static Token tokenize_word(const char *p) {
  Token t = {0};
  t.lex.ptr = p;
  t.kind = TK_IDENT;

  while (isalnum(*p) || *p == '_') {
    ++p;
  }
  t.lex.len = p - t.lex.ptr;

  switch (t.lex.len) {
  case 2:
    if (strncmp(t.lex.ptr, "if", 2) == 0) {
      t.kind = TK_KW_IF;
    }
    break;
  case 3:
    if (strncmp(t.lex.ptr, "for", 3) == 0) {
      t.kind = TK_KW_FOR;
    }
    break;
  case 4:
    if (strncmp(t.lex.ptr, "else", 4) == 0) {
      t.kind = TK_KW_ELSE;
    }
    break;
  case 5:
    if (strncmp(t.lex.ptr, "while", 5) == 0) {
      t.kind = TK_KW_WHILE;
    }
    break;
  case 6:
    if (strncmp(t.lex.ptr, "return", 6) == 0) {
      t.kind = TK_KW_RETURN;
    }
    break;
  }

  return t;
}

static Token tokenize_numeric_literal(const char *p) {
  Token t = {0};
  t.kind = TK_NUM;
  t.lex.ptr = p;
  t.v.num = strtol(p, (char **)&p, 10);
  t.lex.len = p - t.lex.ptr;
  return t;
}

static Token tokenize(const char *p) {
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

Lexer lexer_create(const char *ptr) {
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
