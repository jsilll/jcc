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

// -----------------------------------------------------------------------------
// Error handling
// -----------------------------------------------------------------------------

static void error(char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  vfprintf(stderr, fmt, ap);
  va_end(ap);
  exit(EXIT_FAILURE);
}

static void verror_at(char *start, char *loc, char *fmt, va_list ap) {
  int pos = loc - start;
  fprintf(stderr, "%s\n", start);
  fprintf(stderr, "%*s", pos, "");
  fprintf(stderr, "^ ");
  vfprintf(stderr, fmt, ap);
  fprintf(stderr, "\n");
}

static void error_at(char *start, char *loc, char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  verror_at(start, loc, fmt, ap);
}

// -----------------------------------------------------------------------------
// Tokenizer
// -----------------------------------------------------------------------------

typedef enum {
  TK_EOF, // End of file
  TK_ERR, // Error

  TK_NUM, // Number

  TK_PLUS,  // '+'
  TK_MINUS, // '-'
  TK_MUL,   // '*'
  TK_DIV,   // '/'
} TokenKind;

typedef struct {
  TokenKind kind;
  char *lex;
  u32 len;
  i32 val;
} Token;

static Token tokenize(char *p) {
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

    // Error
    t.kind = TK_ERR;
    t.lex = p;
    t.len = 1;
    return t;
  }

  t.kind = TK_EOF;
  return t;
}

// -----------------------------------------------------------------------------
// Parser
// -----------------------------------------------------------------------------

typedef enum {
  ND_NUM, // Number
  ND_ADD, // '+'
  ND_SUB, // '-'
  ND_DIV, // '/'
  ND_MUL, // '*
} NodeKind;

typedef struct Node {
  NodeKind kind;    // Node kind
  struct Node *lhs; // Left-hand side
  struct Node *rhs; // Right-hand side
  i32 val;          // Number
} Node;

static Node *new_node(NodeKind kind) {
  Node *node = calloc(1, sizeof(Node));
  node->kind = kind;
  return node;
}

static Node *new_node_num(i32 val) {
  Node *node = new_node(ND_NUM);
  node->val = val;
  return node;
}

static Node *new_node_binary(NodeKind kind, Node *lhs, Node *rhs) {
  Node *node = new_node(kind);
  node->lhs = lhs;
  node->rhs = rhs;
  return node;
}

typedef enum {
  PREC_LOW = 1,
  PREC_ADD = 2, // '+' and '-'
  PREC_MUL = 3, // '*' and '/'
} Precedence;

// static parse_expr(Token *t, Precedence prec) {}

// -----------------------------------------------------------------------------
// Main
// -----------------------------------------------------------------------------

int main(int argc, char *argv[]) {
  if (argc != 2) {
    error("usage: %s <expr>\n", argv[0]);
    return EXIT_FAILURE;
  }

  char *start = argv[1];
  Token t = tokenize(argv[1]);

  printf("  .globl main\n");
  printf("main:\n");

  if (t.kind != TK_NUM) {
    error_at(start, t.lex, "expected a number, but got '%.*s'", t.len, t.lex);
    return EXIT_FAILURE;
  }

  printf("  mov $%d, %%rax\n", t.val);

  t = tokenize(t.lex + t.len);
  while (true) {
    switch (t.kind) {
    case TK_PLUS:
      t = tokenize(t.lex + t.len);
      if (t.kind != TK_NUM) {
        error_at(start, t.lex, "expected a number, but got '%.*s'\n", t.len,
                 t.lex);
        return EXIT_FAILURE;
      }
      printf("  add $%d, %%rax\n", t.val);
      t = tokenize(t.lex + t.len);
      break;
    case TK_MINUS:
      t = tokenize(t.lex + t.len);
      if (t.kind != TK_NUM) {
        error_at(start, t.lex, "expected a number, but got '%.*s'\n", t.len,
                 t.lex);
        return EXIT_FAILURE;
      }
      printf("  sub $%d, %%rax\n", t.val);
      t = tokenize(t.lex + t.len);
      break;
    case TK_NUM:
      error_at(start, t.lex, "unexpected number: '%d'\n", t.val);
      return EXIT_FAILURE;
    case TK_ERR:
    case TK_MUL:
    case TK_DIV:
      error_at(start, t.lex, "unexpected token: '%.*s'\n", t.len, t.lex);
      return EXIT_FAILURE;
    case TK_EOF:
      printf("  ret\n");
      return EXIT_SUCCESS;
    }
  }
}
