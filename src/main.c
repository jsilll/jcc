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
      t.lex.ptr = p;
      t.val.i = strtol(p, &p, 10);
      t.lex.len = p - t.lex.ptr;
      return t;
    }

    // Punctuation
    switch (*p) {
    case '+':
      t.kind = TK_PLUS;
      t.lex.ptr = p;
      t.lex.len = 1;
      ++p;
      return t;
    case '-':
      t.kind = TK_MINUS;
      t.lex.ptr = p;
      t.lex.len = 1;
      ++p;
      return t;
    case '*':
      t.kind = TK_MUL;
      t.lex.ptr = p;
      t.lex.len = 1;
      ++p;
      return t;
    case '/':
      t.kind = TK_DIV;
      t.lex.ptr = p;
      t.lex.len = 1;
      ++p;
      return t;
    case '(':
      t.kind = TK_LPAREN;
      t.lex.ptr = p;
      t.lex.len = 1;
      ++p;
      return t;
    case ')':
      t.kind = TK_RPAREN;
      t.lex.ptr = p;
      t.lex.len = 1;
      ++p;
      return t;
    }

    // Error
    t.kind = TK_ERR;
    t.lex.ptr = p;
    t.lex.len = 1;
    return t;
  }

  t.kind = TK_EOF;
  return t;
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
  ND_ADD, // '+'
  ND_SUB, // '-'
  ND_MUL, // '*
  ND_DIV, // '/'
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

static Node *new_node(Arena *a, NodeKind kind) {
  Node *node = arena_alloc(a, sizeof(Node));
  node->kind = kind;
  return node;
}

static Node *new_node_err(Arena *a, Token *t) {
  Node *node = new_node(a, ND_ERR);
  node->lex = t->lex;
  return node;
}

static Node *new_node_num(Arena *a, Token *t) {
  Node *node = new_node(a, ND_NUM);
  node->val.i = t->val.i;
  node->lex = t->lex;
  return node;
}

static Node *new_node_unary(Arena *a, Token *t, NodeKind kind, Node *lhs) {
  Node *node = new_node(a, kind);
  node->lex = t->lex;
  node->lhs = lhs;
  return node;
}

static Node *new_node_binary(Arena *a, Token *t, NodeKind kind, Node *lhs,
                             Node *rhs) {
  Node *node = new_node(a, kind);
  node->lex = t->lex;
  node->lhs = lhs;
  node->rhs = rhs;
  return node;
}

typedef enum {
  PREC_LOW = 1,
  PREC_ADD = 2, // '+' and '-'
  PREC_MUL = 3, // '*' and '/'
} Precedence;

static Node *parse_expr(Arena *a, Lexer *l, Precedence p);

static Node *parse_primary(Arena *a, Lexer *l) {
  Token *t = lexer_next(l);
  switch (t->kind) {
  case TK_NUM:
    return new_node_num(a, t);
  case TK_MINUS:
    return new_node_unary(a, t, ND_SUB, parse_expr(a, l, PREC_LOW));
  case TK_LPAREN: {
    Node *node = parse_expr(a, l, PREC_LOW);
    if (lexer_peek(l)->kind != TK_RPAREN) {
      return new_node_err(a, lexer_peek(l));
    }
    lexer_next(l);
    return node;
  }
  default:
    return new_node_err(a, t);
  }
}

static Node *parse_expr(Arena *a, Lexer *l, Precedence p) {
  Node *lhs = parse_primary(a, l);
  while (true) {
    Token *t = lexer_peek(l);
    switch (l->token.kind) {
    case TK_PLUS:
      if (p >= PREC_ADD) {
        return lhs;
      }
      lexer_next(l);
      lhs = new_node_binary(a, t, ND_ADD, lhs, parse_expr(a, l, PREC_ADD));
      break;
    case TK_MINUS:
      if (p >= PREC_ADD) {
        return lhs;
      }
      lexer_next(l);
      lhs = new_node_binary(a, t, ND_SUB, lhs, parse_expr(a, l, PREC_ADD));
      break;
    case TK_MUL:
      if (p >= PREC_MUL) {
        return lhs;
      }
      lexer_next(l);
      lhs = new_node_binary(a, t, ND_MUL, lhs, parse_expr(a, l, PREC_MUL));
      break;
    case TK_DIV:
      if (p >= PREC_MUL) {
        return lhs;
      }
      lexer_next(l);
      lhs = new_node_binary(a, t, ND_DIV, lhs, parse_expr(a, l, PREC_MUL));
      break;
    default:
      return lhs;
    }
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
  switch (node->kind) {
  case ND_NUM:
    printf("  mov $%d, %%rax\n", node->val.i);
    return;
  case ND_ADD:
    gen_asm(node->rhs);
    printf("  push %%rax\n");
    gen_asm(node->lhs);
    printf("  pop %%rdi\n");
    printf("  add %%rdi, %%rax\n");
    return;
  case ND_SUB:
    gen_asm(node->rhs);
    printf("  push %%rax\n");
    gen_asm(node->lhs);
    printf("  pop %%rdi\n");
    printf("  sub %%rdi, %%rax\n");
    return;
  case ND_MUL:
    gen_asm(node->rhs);
    printf("  push %%rax\n");
    gen_asm(node->lhs);
    printf("  pop %%rdi\n");
    printf("  imul %%rdi, %%rax\n");
    return;
  case ND_DIV:
    gen_asm(node->rhs);
    printf("  push %%rax\n");
    gen_asm(node->lhs);
    printf("  pop %%rdi\n");
    printf("  cqo\n");
    printf("  idiv %%rdi\n");
    return;
  case ND_ERR:
    error_at(node->lex.ptr, node->lex.ptr + node->lex.len, "unexpected token");
    return;
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
    error_at(start, lexer.token.lex.ptr, "unexpected token: '%.*s'\n",
             lexer.token.lex.len, lexer.token.lex.ptr);
    return EXIT_FAILURE;
  }

  printf("  .globl main\n");
  printf("main:\n");
  gen_asm(node);
  printf("  ret\n");

  arena_destroy(&arena);

  return 0;
}
