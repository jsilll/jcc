#include <ctype.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

typedef int8_t i8;
typedef int16_t i16;
typedef int32_t i32;
typedef int64_t i64;

typedef uint8_t u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;

static void Error(char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  vfprintf(stderr, fmt, ap);
  va_end(ap);
  exit(EXIT_FAILURE);
}

typedef enum {
  TK_EOF,
  TK_NUM,
  TK_PLUS,
  TK_MINUS,
} TokenKind;

typedef struct {
  TokenKind kind;
  char *lex;
  u32 len;
  i32 val;
} Token;

static Token Tokenize(char *p) {
  Token t = {0};
  while (*p) {
    // Skip whitespace
    if (isspace(*p)) {
      ++p;
      continue;
    }

    // Numeric literal
    if (isdigit(*p)) {
      t.kind = TK_NUM;
      t.lex = p;
      t.val = strtol(p, &p, 10);
      t.len = p - t.lex;
      return t;
    }

    // Punctuation
    switch (*p) {
    case '+':
      t.kind = TK_PLUS;
      t.lex = p;
      t.len = 1;
      ++p;
      return t;
    case '-':
      t.kind = TK_MINUS;
      t.lex = p;
      t.len = 1;
      ++p;
      return t;
    }

    Error("unexpected character: '%c'\n", *p);
  }

  t.kind = TK_EOF;
  return t;
}

int main(int argc, char *argv[]) {
  if (argc != 2) {
    Error("usage: %s <expr>\n", argv[0]);
    return EXIT_FAILURE;
  }

  Token t = Tokenize(argv[1]);

  printf("  .globl main\n");
  printf("main:\n");

  if (t.kind != TK_NUM) {
    Error("expected a number, but got %.*s\n", t.len, t.lex);
    return EXIT_FAILURE;
  }

  printf("  mov $%d, %%rax\n", t.val);

  t = Tokenize(t.lex + t.len);
  while (true) {
    switch (t.kind) {
    case TK_PLUS:
      t = Tokenize(t.lex + t.len);
      if (t.kind != TK_NUM) {
        Error("expected a number, but got %.*s\n", t.len, t.lex);
        return EXIT_FAILURE;
      }
      printf("  add $%d, %%rax\n", t.val);
      t = Tokenize(t.lex + t.len);
      break;
    case TK_MINUS:
      t = Tokenize(t.lex + t.len);
      if (t.kind != TK_NUM) {
        Error("expected a number, but got %.*s\n", t.len, t.lex);
        return EXIT_FAILURE;
      }
      printf("  sub $%d, %%rax\n", t.val);
      t = Tokenize(t.lex + t.len);
      break;
    case TK_NUM:
      Error("unexpected number: %d\n", t.val);
      return EXIT_FAILURE;
    case TK_EOF:
      printf("  ret\n");
      return EXIT_SUCCESS;
    }
  }
}
