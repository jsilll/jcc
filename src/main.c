// -----------------------------------------------------------------------------
// Prelude
// -----------------------------------------------------------------------------

#include <ctype.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

// -----------------------------------------------------------------------------
// Debug macros
// -----------------------------------------------------------------------------

#define DEBUG(msg) fprintf(stderr, "%s:%d: " msg "\n", __FILE__, __LINE__)

#define DEBUGF(fmt, ...)                                                       \
  fprintf(stderr, "%s:%d: " fmt "\n", __FILE__, __LINE__, __VA_ARGS__)

// -----------------------------------------------------------------------------
// Platform
// -----------------------------------------------------------------------------

#if defined(__GNUC__)
#define CC_COMPILER_GCC
#elif defined(_MSC_VER)
#define CC_COMPILER_MSVC
#elif defined(__clang__)
#define CC_COMPILER_CLANG
#else
#error "Unsupported compiler"
#endif

#if defined(__linux__)
#define CC_TARGET_LINUX
#elif defined(_WIN32) || defined(_WIN64)
#define CC_TARGET_WINDOWS
#elif defined(__APPLE__) || defined(__MACH__)
#define CC_TARGET_APPLE_OSX
#else
#error "Unsupported target"
#endif

// -----------------------------------------------------------------------------
// Memory
// -----------------------------------------------------------------------------

#if defined(CC_TARGET_LINUX)
#include <malloc.h>
#elif defined(CC_TARGET_WINDOWS)
#include <malloc.h>
#elif defined(CC_TARGET_APPLE_OSX)
#include <stdlib.h>
#else
#error "Unsupported target"
#endif

#ifndef CC_MEMORY_L1_CACHE_LINE_SIZE
#define CC_MEMORY_L1_CACHE_LINE_SIZE 64
#endif

static inline void *alloc_aligned(size_t size) {
#if defined(CC_TARGET_LINUX)
  return memalign(CC_MEMORY_L1_CACHE_LINE_SIZE, size);
#elif defined(CC_TARGET_WINDOWS)
  return _aligned_malloc(size, CC_MEMORY_L1_CACHE_LINE_SIZE);
#elif defined(CC_TARGET_APPLE_OSX)
  void *ptr = NULL;
  if (posix_memalign(&ptr, CC_MEMORY_L1_CACHE_LINE_SIZE, size) != 0) {
    return NULL;
  }
  return ptr;
#else
#error "Unsupported target"
#endif
}

static inline void free_aligned(void *ptr) {
#if defined(CC_TARGET_WINDOWS)
  _aligned_free(ptr);
#else
  free(ptr);
#endif
}

// -----------------------------------------------------------------------------
// Arena
// -----------------------------------------------------------------------------

#include <assert.h>
#include <stddef.h>
#include <string.h>

typedef struct ArenaBlock {
  uint8_t *ptr;            // Pointer to the block
  size_t size;             // Size of the block
  struct ArenaBlock *next; // Next block
} ArenaBlock;

typedef struct {
  uint8_t *ptr;          // Pointer to the current block
  size_t default_size;   // Default block size
  size_t commited_size;  // Current block committed size
  size_t allocated_size; // Current block allocated size
  ArenaBlock *used;      // Used blocks
  ArenaBlock *free;      // Free blocks
} Arena;

static Arena arena_default(void) {
  Arena arena = {0};
  arena.default_size = 4096;
  return arena;
}

static Arena arena_create(size_t default_size) {
  Arena arena = {0};
  arena.default_size = default_size;
  return arena;
}

static void arena_destroy(Arena *arena) {
  if (arena->ptr != NULL) {
    free_aligned(arena->ptr);
  }
  ArenaBlock *block = arena->used;
  while (block != NULL) {
    ArenaBlock *next = block->next;
    free_aligned(block->ptr);
    free(block);
    block = next;
  }
  block = arena->free;
  while (block != NULL) {
    ArenaBlock *next = block->next;
    free_aligned(block->ptr);
    free(block);
    block = next;
  }
}

static void arena_clear(Arena *arena) {
  arena->commited_size = 0;
  ArenaBlock *block = arena->free;
  ArenaBlock *prev = NULL;
  while (block != NULL) {
    prev = block;
    block = block->next;
  }
  if (prev != NULL) {
    prev->next = arena->used;
  } else {
    arena->free = arena->used;
  }
  arena->used = NULL;
}

static size_t arena_total_allocated(Arena *arena) {
  size_t total = arena->allocated_size;
  ArenaBlock *block = arena->used;
  while (block != NULL) {
    total += block->size;
    block = block->next;
  }
  block = arena->free;
  while (block != NULL) {
    total += block->size;
    block = block->next;
  }
  return total;
}

static void *arena_alloc(Arena *arena, size_t size) {
  static_assert(sizeof(max_align_t) != 0 &&
                    (sizeof(max_align_t) & (sizeof(max_align_t) - 1)) == 0,
                "Alignment must be a power of two");
  size = (size + sizeof(max_align_t) - 1) & ~(sizeof(max_align_t) - 1);
  if (arena->commited_size + size > arena->allocated_size) {
    if (arena->ptr != NULL) {
      ArenaBlock *block = malloc(sizeof(ArenaBlock));
      block->ptr = arena->ptr;
      block->size = arena->allocated_size;
      block->next = arena->used;
      arena->used = block;
    }
    ArenaBlock *block = arena->free;
    ArenaBlock *prev = NULL;
    while (block != NULL) {
      if (block->size >= size) {
        break;
      }
      prev = block;
      block = block->next;
    }
    if (block == NULL) {
      arena->allocated_size =
          size > arena->default_size ? size : arena->default_size;
      arena->ptr = alloc_aligned(arena->allocated_size);
    } else if (prev != NULL) {
      prev->next = block->next;
      arena->allocated_size = block->size;
      arena->ptr = block->ptr;
    } else {
      arena->free = block->next;
      arena->allocated_size = block->size;
      arena->ptr = block->ptr;
    }
    arena->commited_size = 0;
  }
  void *ptr = arena->ptr + arena->commited_size;
  arena->commited_size += size;
  return ptr;
}

// -----------------------------------------------------------------------------
// String view
// -----------------------------------------------------------------------------

typedef struct {
  char *ptr;
  size_t len;
} StringView;

// -----------------------------------------------------------------------------
// Tokenizer
// -----------------------------------------------------------------------------

typedef enum {
  TK_ERR,    // Error
  TK_EOF,    // End of file
  TK_NUM,    // Number
  TK_PLUS,   // '+'
  TK_MINUS,  // '-'
  TK_MUL,    // '*'
  TK_DIV,    // '/'
  TK_EQ,     // '=='
  TK_NE,     // '!='
  TK_LT,     // '<'
  TK_LE,     // '<='
  TK_GT,     // '>'
  TK_GE,     // '>='
  TK_LPAREN, // '('
  TK_RPAREN, // ')'
} TokenKind;

typedef struct {
  StringView lex;
  TokenKind kind;
  union {
    float f;
    int32_t i;
  } val;
} Token;

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

static Token tokenize(char *p) {
  while (*p) {
    // Skip whitespace
    if (isspace(*p)) {
      ++p;
      continue;
    }

    // Numeric literal
    if (isdigit(*p)) {
      Token t = {0};
      t.kind = TK_NUM;
      t.lex.ptr = p;
      t.val.i = strtol(p, &p, 10);
      t.lex.len = p - t.lex.ptr;
      return t;
    }

    // Punctuation
    switch (*p) {
    case '+':
      return tokenize_single_char(p, TK_PLUS);
    case '-':
      return tokenize_single_char(p, TK_MINUS);
    case '*':
      return tokenize_single_char(p, TK_MUL);
    case '/':
      return tokenize_single_char(p, TK_DIV);
    case '(':
      return tokenize_single_char(p, TK_LPAREN);
    case ')':
      return tokenize_single_char(p, TK_RPAREN);
    case '=':
      return tokenize_double_char(p, '=', TK_EQ, TK_ERR);
    case '!':
      return tokenize_double_char(p, '=', TK_NE, TK_ERR);
    case '<':
      return tokenize_double_char(p, '=', TK_LE, TK_LT);
    case '>':
      return tokenize_double_char(p, '=', TK_GE, TK_GT);
    }

    return tokenize_single_char(p, TK_ERR);
  }

  return tokenize_single_char(p, TK_EOF);
}

typedef struct {
  bool consumed;
  char *ptr;
  Token token;
} Lexer;

static Lexer lexer_create(char *ptr) {
  Lexer l = {0};
  l.consumed = true;
  l.ptr = ptr;
  return l;
}

static Token *lexer_peek(Lexer *l) {
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

static Token *lexer_next(Lexer *l) {
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

// -----------------------------------------------------------------------------
// Parser
// -----------------------------------------------------------------------------

typedef enum {
  ND_ERR, // Error
  ND_NUM, // Number
  ND_NEG, // '-'
  ND_ADD, // '+'
  ND_SUB, // '-'
  ND_MUL, // '*
  ND_DIV, // '/'
  ND_EQ,  // '=='
  ND_NE,  // '!='
  ND_LT,  // '<'
  ND_LE,  // '<='
  ND_GT,  // '>'
  ND_GE,  // '>='
} NodeKind;

typedef struct Node {
  NodeKind kind;    // Node kind
  StringView lex;   // Token lexeme
  struct Node *lhs; // Left-hand side
  struct Node *rhs; // Right-hand side
  union {
    float f;
    int32_t i;
  } val;
} Node;

static Node *node_create(Arena *a, NodeKind kind) {
  Node *node = arena_alloc(a, sizeof(Node));
  node->kind = kind;
  return node;
}

static Node *node_create_err(Arena *a, Token *t) {
  Node *node = node_create(a, ND_ERR);
  node->lex = t->lex;
  return node;
}

static Node *node_create_num(Arena *a, Token *t) {
  Node *node = node_create(a, ND_NUM);
  node->val.i = t->val.i;
  node->lex = t->lex;
  return node;
}

static Node *node_create_unary(Arena *a, Token *t, NodeKind kind, Node *lhs) {
  Node *node = node_create(a, kind);
  node->lex = t->lex;
  node->lhs = lhs;
  return node;
}

static Node *node_create_binary(Arena *a, Token *t, NodeKind kind, Node *lhs,
                                Node *rhs) {
  Node *node = node_create(a, kind);
  node->lex = t->lex;
  node->lhs = lhs;
  node->rhs = rhs;
  return node;
}

typedef enum {
  PREC_LOW = 1,
  PREC_REL = 2, // '<' and '>'
  PREC_EQ = 3,  // '==' and '!='
  PREC_ADD = 4, // '+' and '-'
  PREC_MUL = 5, // '*' and '/'
  PREC_HIGH = 6,
} Precedence;

typedef struct {
  NodeKind kind;
  Precedence prec;
} PrecedencePair;

static PrecedencePair get_precedence(TokenKind kind) {
  switch (kind) {
  case TK_PLUS:
    return (PrecedencePair){ND_ADD, PREC_ADD};
  case TK_MINUS:
    return (PrecedencePair){ND_SUB, PREC_ADD};
  case TK_MUL:
    return (PrecedencePair){ND_MUL, PREC_MUL};
  case TK_DIV:
    return (PrecedencePair){ND_DIV, PREC_MUL};
  case TK_EQ:
    return (PrecedencePair){ND_EQ, PREC_MUL};
  case TK_NE:
    return (PrecedencePair){ND_NE, PREC_EQ};
  case TK_LT:
    return (PrecedencePair){ND_LT, PREC_REL};
  case TK_LE:
    return (PrecedencePair){ND_LE, PREC_REL};
  case TK_GT:
    return (PrecedencePair){ND_GT, PREC_REL};
  case TK_GE:
    return (PrecedencePair){ND_GE, PREC_REL};
  default:
    return (PrecedencePair){ND_ERR, PREC_LOW};
  }
}

static Node *parse_expr(Arena *a, Lexer *l, Precedence p);

static Node *parse_primary(Arena *a, Lexer *l) {
  Token *t = lexer_next(l);
  switch (t->kind) {
  case TK_NUM:
    return node_create_num(a, t);
  case TK_PLUS:
    return parse_primary(a, l);
  case TK_MINUS:
    return node_create_unary(a, t, ND_NEG, parse_expr(a, l, PREC_HIGH));
  case TK_LPAREN: {
    Node *node = parse_expr(a, l, PREC_LOW);
    if (lexer_peek(l)->kind != TK_RPAREN) {
      return node_create_err(a, lexer_peek(l));
    }
    lexer_next(l);
    return node;
  }
  default:
    return node_create_err(a, t);
  }
}

static Node *parse_expr(Arena *a, Lexer *l, Precedence p) {
  Node *lhs = parse_primary(a, l);
  while (true) {
    Token *t = lexer_peek(l);
    PrecedencePair pp = get_precedence(t->kind);
    if (pp.kind == ND_ERR || p >= pp.prec) {
      return lhs;
    }
    lexer_next(l);
    lhs = node_create_binary(a, t, pp.kind, lhs, parse_expr(a, l, pp.prec));
  }
  return lhs;
}

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
// Code generation
// -----------------------------------------------------------------------------

static void gen_asm(Node *node) {
  if (node->kind == ND_NUM) {
    printf("  mov $%d, %%rax\n", node->val.i);
    return;
  }
  if (node->kind == ND_NEG) {
    gen_asm(node->lhs);
    printf("  neg %%rax\n");
    return;
  }
  gen_asm(node->rhs);
  printf("  push %%rax\n");
  gen_asm(node->lhs);
  printf("  pop %%rdi\n");
  switch (node->kind) {
  case ND_ADD:
    printf("  add %%rdi, %%rax\n");
    break;
  case ND_SUB:
    printf("  sub %%rdi, %%rax\n");
    break;
  case ND_MUL:
    printf("  imul %%rdi, %%rax\n");
    break;
  case ND_DIV:
    printf("  cqo\n");
    printf("  idiv %%rdi\n");
    break;
  case ND_EQ:
    printf("  cmp %%rdi, %%rax\n");
    printf("  sete %%al\n");
    printf("  movzb %%al, %%rax\n");
    break;
  case ND_NE:
    printf("  cmp %%rdi, %%rax\n");
    printf("  setne %%al\n");
    printf("  movzb %%al, %%rax\n");
    break;
  case ND_LT:
    printf("  cmp %%rdi, %%rax\n");
    printf("  setl %%al\n");
    printf("  movzb %%al, %%rax\n");
    break;
  case ND_LE:
    printf("  cmp %%rdi, %%rax\n");
    printf("  setle %%al\n");
    printf("  movzb %%al, %%rax\n");
    break;
  case ND_GT:
    printf("  cmp %%rdi, %%rax\n");
    printf("  setg %%al\n");
    printf("  movzb %%al, %%rax\n");
    break;
  case ND_GE:
    printf("  cmp %%rdi, %%rax\n");
    printf("  setge %%al\n");
    printf("  movzb %%al, %%rax\n");
    break;
  case ND_NEG:
  case ND_NUM:
  case ND_ERR:
    error_at(node->lex.ptr, node->lex.ptr + node->lex.len,
             "unexpected token during code generation '%.*s'\n",
             (int)node->lex.len, node->lex.ptr);
    break;
  }
}

// -----------------------------------------------------------------------------
// Main
// -----------------------------------------------------------------------------

int main(int argc, char *argv[]) {
  if (argc != 2) {
    error("usage: %s <expr>\n", argv[0]);
    return EXIT_FAILURE;
  }

  char *start = argv[1];
  Arena arena = arena_default();

  Lexer lexer = lexer_create(start);
  while (lexer_peek(&lexer)->kind != TK_EOF) {
    switch (lexer_peek(&lexer)->kind) {
    case TK_PLUS:
      fprintf(stderr, "TK_PLUS\n");
      break;
    case TK_MINUS:
      fprintf(stderr, "TK_MINUS\n");
      break;
    case TK_MUL:
      fprintf(stderr, "TK_MUL\n");
      break;
    case TK_DIV:
      fprintf(stderr, "TK_DIV\n");
      break;
    case TK_EQ:
      fprintf(stderr, "TK_EQ\n");
      break;
    case TK_NE:
      fprintf(stderr, "TK_NE\n");
      break;
    case TK_LT:
      fprintf(stderr, "TK_LT\n");
      break;
    case TK_LE:
      fprintf(stderr, "TK_LE\n");
      break;
    case TK_GT:
      fprintf(stderr, "TK_GT\n");
      break;
    case TK_GE:
      fprintf(stderr, "TK_GE\n");
      break;
    case TK_LPAREN:
      fprintf(stderr, "TK_LPAREN\n");
      break;
    case TK_RPAREN:
      fprintf(stderr, "TK_RPAREN\n");
      break;
    case TK_NUM:
      fprintf(stderr, "TK_NUM: %d\n", lexer_peek(&lexer)->val.i);
      break;
    case TK_ERR:
      fprintf(stderr, "TK_ERR: '%.*s'\n", (int)lexer_peek(&lexer)->lex.len,
              lexer_peek(&lexer)->lex.ptr);
      break;
    case TK_EOF:
      fprintf(stderr, "TK_EOF\n");
      break;
    }
    lexer_next(&lexer);
  }

  lexer = lexer_create(start);
  Node *node = parse_expr(&arena, &lexer, PREC_LOW);

  if (lexer.token.kind != TK_EOF) {
    error_at(start, lexer.token.lex.ptr,
             "unexpected token during parsing: '%.*s'\n", lexer.token.lex.len,
             lexer.token.lex.ptr);
    return EXIT_FAILURE;
  }

  printf("  .globl main\n");
  printf("main:\n");
  gen_asm(node);
  printf("  ret\n");

  arena_destroy(&arena);

  return 0;
}
