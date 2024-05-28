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
// Enum macros
// -----------------------------------------------------------------------------

#define GENERATE_ENUM(ENUM) ENUM,

#define GENERATE_STRING(STRING) #STRING,

// -----------------------------------------------------------------------------
// Debug macros
// -----------------------------------------------------------------------------

#define DEBUG(msg) fprintf(stderr, "%s:%d: " msg "\n", __FILE__, __LINE__)

#define DEBUGF(fmt, ...)                                                       \
  fprintf(stderr, "%s:%d: " fmt "\n", __FILE__, __LINE__, __VA_ARGS__)

// -----------------------------------------------------------------------------
// Error handling
// -----------------------------------------------------------------------------

static void error(char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  vfprintf(stderr, fmt, ap);
  va_end(ap);
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
// Arena
// -----------------------------------------------------------------------------

#include "arena.h"

// -----------------------------------------------------------------------------
// String view
// -----------------------------------------------------------------------------

typedef struct {
  char *ptr;
  uint32_t len;
} StringView;

// possible useful methods for StringView
// - starts_with
// - ends_with
// - contains
// - find
// - rfind
// - split

// -----------------------------------------------------------------------------
// Lexer
// -----------------------------------------------------------------------------

#include <string.h>

#define ENUMERATE_TOKENS(M)                                                    \
  M(TK_ERR)                                                                    \
  M(TK_EOF)                                                                    \
  M(TK_SEMI)                                                                   \
  M(TK_LPAREN)                                                                 \
  M(TK_RPAREN)                                                                 \
  M(TK_LBRACE)                                                                 \
  M(TK_RBRACE)                                                                 \
  M(TK_PLUS)                                                                   \
  M(TK_MINUS)                                                                  \
  M(TK_MUL)                                                                    \
  M(TK_DIV)                                                                    \
  M(TK_EQ)                                                                     \
  M(TK_EQEQ)                                                                   \
  M(TK_NE)                                                                     \
  M(TK_LT)                                                                     \
  M(TK_LE)                                                                     \
  M(TK_GT)                                                                     \
  M(TK_GE)                                                                     \
  M(TK_NUM)                                                                    \
  M(TK_RETURN)                                                                 \
  M(TK_IDENT)

typedef enum { ENUMERATE_TOKENS(GENERATE_ENUM) } TokenKind;

static const char *TOKEN_KIND_STR[] = {ENUMERATE_TOKENS(GENERATE_STRING)};

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
  t.val.i = strtol(p, &p, 10);
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

    // Numeric literal
    if (isdigit(*p)) {
      return tokenize_numeric_literal(p);
    }

    // Identifier and keywords
    if (isalpha(*p) || *p == '_') {
      return tokenize_word(p);
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

typedef struct Object {
  StringView lex;      // Token lexeme
  int32_t offset;      // Offset from RBP
  struct Object *next; // Next object
} Object;

static Object *object_create_local(Arena *a, Object *all, StringView lex) {
  Object *o = arena_alloc(a, sizeof(Object));
  o->lex = lex;
  o->next = all;
  return o;
}

#define ENUMERATE_EXPRS(M)                                                     \
  M(EXPR_NUM)                                                                  \
  M(EXPR_VAR)                                                                  \
  M(EXPR_NEG)                                                                  \
  M(EXPR_ADD)                                                                  \
  M(EXPR_SUB)                                                                  \
  M(EXPR_MUL)                                                                  \
  M(EXPR_DIV)                                                                  \
  M(EXPR_EQ)                                                                   \
  M(EXPR_NE)                                                                   \
  M(EXPR_LT)                                                                   \
  M(EXPR_LE)                                                                   \
  M(EXPR_GT)                                                                   \
  M(EXPR_GE)                                                                   \
  M(EXPR_ASSIGN)

typedef enum { ENUMERATE_EXPRS(GENERATE_ENUM) } ExprKind;

static const char *EXPR_KIND_STR[] = {ENUMERATE_EXPRS(GENERATE_STRING)};

#define ERROR_EXPR_KIND(node)                                                  \
  error(                                                                       \
      "unexpected expression kind during %s:\n-> kind: %s\n-> lex: '%.*s'\n",  \
      __FUNCTION__, EXPR_KIND_STR[node->tag.expr], (int)node->lex.len,         \
      node->lex.ptr)

#define ENUMERATE_STMTS(M)                                                     \
  M(STMT_EXPR)                                                                 \
  M(STMT_RETURN)                                                               \
  M(STMT_BLOCK)

typedef enum { ENUMERATE_STMTS(GENERATE_ENUM) } StmtKind;

static const char *STMT_KIND_STR[] = {ENUMERATE_STMTS(GENERATE_STRING)};

#define ERROR_STMT_KIND(node)                                                  \
  error("unexpected statement kind during %s:\n-> kind: %s\n-> lex: '%.*s'\n", \
        __FUNCTION__, STMT_KIND_STR[node->tag.stmt], (int)node->lex.len,       \
        node->lex.ptr)

#define ENUMERATE_NODES(M)                                                     \
  M(ND_ERR)                                                                    \
  M(ND_EXPR)                                                                   \
  M(ND_STMT)

typedef enum { ENUMERATE_NODES(GENERATE_ENUM) } NodeKind;

static const char *NODE_KIND_STR[] = {ENUMERATE_NODES(GENERATE_STRING)};

#define ERROR_NODE_KIND(node)                                                  \
  error("unexpected node kind during %s:\n-> kind: %s\n-> lex: '%.*s'\n",      \
        __FUNCTION__, NODE_KIND_STR[node->kind], (int)node->lex.len,           \
        node->lex.ptr)

typedef struct Node {
  NodeKind kind;
  union {
    ExprKind expr;
    StmtKind stmt;
  } tag;

  StringView lex;    // Token lexeme
  struct Node *lhs;  // Left-hand side
  struct Node *rhs;  // Right-hand side
  struct Node *next; // Next node
  union {
    float f;
    int32_t i;
    Object *obj;
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

static Node *node_create_expr_num(Arena *a, Token *t) {
  Node *node = node_create(a, ND_EXPR);
  node->tag.expr = EXPR_NUM;
  node->val.i = t->val.i;
  node->lex = t->lex;
  return node;
}

static Node *node_create_expr_var(Arena *a, Object *o) {
  Node *node = node_create(a, ND_EXPR);
  node->tag.expr = EXPR_VAR;
  node->lex = o->lex;
  node->val.obj = o;
  return node;
}

static Node *node_create_expr_unary(Arena *a, Token *t, ExprKind kind,
                                    Node *lhs) {
  Node *node = node_create(a, ND_EXPR);
  node->tag.expr = kind;
  node->lex = t->lex;
  node->lhs = lhs;
  return node;
}

static Node *node_create_expr_binary(Arena *a, Token *t, ExprKind kind,
                                     Node *lhs, Node *rhs) {
  Node *node = node_create(a, ND_EXPR);
  node->tag.expr = kind;
  node->lex = t->lex;
  node->lhs = lhs;
  node->rhs = rhs;
  return node;
}

static Node *node_create_stmt_unary(Arena *a, Token *t, StmtKind kind,
                                    Node *lhs) {
  Node *node = node_create(a, ND_STMT);
  node->tag.stmt = kind;
  node->lex = t->lex;
  node->lhs = lhs;
  return node;
}

#define ENUMERATE_FUNCTIONS(M)                                                 \
  M(FK_ERROR)                                                                  \
  M(FK_MAIN)                                                                   \
  M(FK_FUNC)

typedef enum FunctionKind { ENUMERATE_FUNCTIONS(GENERATE_ENUM) } FunctionKind;

static const char *FUNCTION_KIND_STR[] = {ENUMERATE_FUNCTIONS(GENERATE_STRING)};

#define ERROR_FUNCTION_KIND(f)                                                 \
  error("unexpected function kind during %s:\n-> kind: %s\n", __FUNCTION__,    \
        FUNCTION_KIND_STR[f->kind])

typedef struct {
  FunctionKind kind;
  Node *body;
  Object *locals;
  uint32_t stack_size;
} Function;

static Function *function_create(Arena *a, FunctionKind kind) {
  Function *f = arena_alloc(a, sizeof(Function));
  f->kind = kind;
  return f;
}

// PUMA'S REBL TAC
//
// The following table shows the precedence and associativity of all the C
// operators. The operators are listed in order of precedence, from lowest to
// highest. Operators in the same box have the same precedence and
// associativity.
//
// -----------------------------------------------------------------------------
//   Precedence  |      Operator     |     Description     |   Associativity   |
// -----------------------------------------------------------------------------
//      1        |         ,         |    Expr. separator  |    Left-to-Right  |
// -----------------------------------------------------------------------------
//      2        |         =         |    Assignment       |    Right-to-Left  |
//               |      +=, -=       |    Add/Sub assign   |                   |
//               |      *=, /=       |    Mul/Div assign   |                   |
//               |      %=, &=       |    Mod/BitAnd asgn  |                   |
//               |      ^=, |=       |    BitXOR/BitOR a.  |                   |
//               |    <<=, >>=       |    BitSHL/BitSHR a. |                   |
// -----------------------------------------------------------------------------
//      3        |       ?, :        | Ternary Conditional |    Right-to-Left  |
// -----------------------------------------------------------------------------
//      4        |        ||         |      Logical OR     |    Left-to-Right  |
// -----------------------------------------------------------------------------
//      5        |        &&         |      Logical AND    |    Left-to-Right  |
// -----------------------------------------------------------------------------
//      6        |        |          |        BitOR        |    Left-to-Right  |
// -----------------------------------------------------------------------------
//      7        |        ^          |        BitXOR       |    Left-to-Right  |
// -----------------------------------------------------------------------------
//      8        |        &          |        BitAND       |    Left-to-Right  |
// -----------------------------------------------------------------------------
//      9        |     ==, !=        |  Relational Equal   |    Left-to-Right  |
// -----------------------------------------------------------------------------
//     10        |    >, >=, <, <=   |  Relational Compare |    Left-to-Right  |
// -----------------------------------------------------------------------------
//     11        |     <<, >>        |      Bit Shifts     |    Left-to-Right  |
// -----------------------------------------------------------------------------
//     12        |    +, -           |       Addition      |    Left-to-Right  |
// -----------------------------------------------------------------------------
//     13        |    *, /, %        |    Multiplication   |    Left-to-Right  |
// -----------------------------------------------------------------------------
//     14        |    ++, --         |  Prefix Inc/Dec     |    Right-to-Left  |
//               |     +, -          |  Unary Plus/Minus   |                   |
//               |     !, ~          |  Logical NOT/BitNOT |                   |
//               |     (type)        |    Cast Operator    |                   |
//               |       *           |   Deref Operator    |                   |
//               |       &           |   AddrOf Operator   |                   |
//               |     sizeof        |   SizeOf Operator   |                   |
// -----------------------------------------------------------------------------
//     15        |       ()          |    Function Call    |    Left-to-Right  |
//               |       []          |    Array Subscript  |                   |
//               |       .           |   Struct Member Acc |                   |
//               |       ->          |  Struct Ptr Member  |                   |
//               |      ++, --       |  Postfix Inc/Dec    |                   |
// -----------------------------------------------------------------------------

typedef struct {
  uint8_t lhs;
  uint8_t rhs;
} BindingPower;

typedef struct {
  ExprKind kind;
  BindingPower power;
} BinOpPair;

#define PREC_MIN 0
#define PREC_MAX 11

static BinOpPair try_into_binop(TokenKind kind) {
  switch (kind) {
  case TK_EQ:
    return (BinOpPair){EXPR_ASSIGN, (BindingPower){2, 1}};
  case TK_EQEQ:
    return (BinOpPair){EXPR_EQ, (BindingPower){3, 4}};
  case TK_NE:
    return (BinOpPair){EXPR_NE, (BindingPower){3, 4}};
  case TK_LT:
    return (BinOpPair){EXPR_LT, (BindingPower){5, 6}};
  case TK_LE:
    return (BinOpPair){EXPR_LE, (BindingPower){5, 6}};
  case TK_GT:
    return (BinOpPair){EXPR_GT, (BindingPower){5, 6}};
  case TK_GE:
    return (BinOpPair){EXPR_GE, (BindingPower){5, 6}};
  case TK_PLUS:
    return (BinOpPair){EXPR_ADD, (BindingPower){7, 8}};
  case TK_MINUS:
    return (BinOpPair){EXPR_SUB, (BindingPower){7, 8}};
  case TK_MUL:
    return (BinOpPair){EXPR_MUL, (BindingPower){9, 10}};
  case TK_DIV:
    return (BinOpPair){EXPR_DIV, (BindingPower){9, 10}};
  default:
    return (BinOpPair){EXPR_NUM, (BindingPower){0, 0}};
  }
}

typedef struct {
  Arena *arena;
  Lexer *lexer;
  Object *locals;
} ParseCtx;

static Object *parse_ctx_find_local(ParseCtx *ctx, StringView lex) {
  for (Object *o = ctx->locals; o != NULL; o = o->next) {
    if (o->lex.len == lex.len && strncmp(o->lex.ptr, lex.ptr, lex.len) == 0) {
      return o;
    }
  }
  return NULL;
}

static Node *parse_expr(ParseCtx *ctx, uint8_t prec);

static Node *parse_expr_primary(ParseCtx *ctx) {
  Token *t = lexer_next(ctx->lexer);
  switch (t->kind) {
  case TK_NUM:
    return node_create_expr_num(ctx->arena, t);
  case TK_IDENT: {
    Object *o = parse_ctx_find_local(ctx, t->lex);
    if (o != NULL) {
      return node_create_expr_var(ctx->arena, o);
    }
    ctx->locals = object_create_local(ctx->arena, ctx->locals, t->lex);
    return node_create_expr_var(ctx->arena, ctx->locals);
  }
  case TK_PLUS:
    return parse_expr_primary(ctx);
  case TK_MINUS:
    return node_create_expr_unary(ctx->arena, t, EXPR_NEG,
                                  parse_expr(ctx, PREC_MAX));
  case TK_LPAREN: {
    Node *node = parse_expr(ctx, 0);
    if (lexer_peek(ctx->lexer)->kind != TK_RPAREN) {
      return node_create_err(ctx->arena, lexer_peek(ctx->lexer));
    }
    lexer_next(ctx->lexer);
    return node;
  }
  default:
    return node_create_err(ctx->arena, t);
  }
}

static Node *parse_expr(ParseCtx *ctx, uint8_t prec) {
  Node *lhs = parse_expr_primary(ctx);
  while (true) {
    Token t = *lexer_peek(ctx->lexer);
    BinOpPair bp = try_into_binop(t.kind);
    if (bp.kind == EXPR_NUM || bp.power.lhs <= prec) {
      return lhs;
    }
    lexer_next(ctx->lexer);
    lhs = node_create_expr_binary(ctx->arena, &t, bp.kind, lhs,
                                  parse_expr(ctx, bp.power.rhs));
  }
  return lhs;
}

static Node *parse_stmt(ParseCtx *ctx);

static Node *parse_stmt_block(ParseCtx *ctx) {
  Token t = *lexer_next(ctx->lexer);
  Node head = {0};
  Node *cur = &head;
  while (lexer_peek(ctx->lexer)->kind != TK_RBRACE) {
    cur->next = parse_stmt(ctx);
    cur = cur->next;
  }
  lexer_next(ctx->lexer);
  Node *node = node_create_stmt_unary(ctx->arena, &t, STMT_BLOCK, head.next);
  return node;
}

static Node *parse_stmt(ParseCtx *ctx) {
  switch (lexer_peek(ctx->lexer)->kind) {
  case TK_RETURN: {
    lexer_next(ctx->lexer);
    Node *node = parse_expr(ctx, PREC_MIN);
    if (lexer_peek(ctx->lexer)->kind != TK_SEMI) {
      return node_create_err(ctx->arena, lexer_peek(ctx->lexer));
    }
    return node_create_stmt_unary(ctx->arena, lexer_next(ctx->lexer),
                                  STMT_RETURN, node);
  }
  case TK_LBRACE:
    return parse_stmt_block(ctx);
  default: {
    Node *node = parse_expr(ctx, PREC_MIN);
    if (lexer_peek(ctx->lexer)->kind != TK_SEMI) {
      return node_create_err(ctx->arena, lexer_peek(ctx->lexer));
    }
    return node_create_stmt_unary(ctx->arena, lexer_next(ctx->lexer), STMT_EXPR,
                                  node);
  }
  }
}

static Function *parse(Arena *a, Lexer *l) {
  if (lexer_peek(l)->kind != TK_LBRACE) {
    error("unexpected token during parsing: '%.*s'\n", lexer_peek(l)->lex.len,
          lexer_peek(l)->lex.ptr);
    return function_create(a, FK_ERROR);
  }
  ParseCtx ctx = {a, l, NULL};
  Function *prog = function_create(a, FK_MAIN);
  prog->body = parse_stmt_block(&ctx);
  prog->locals = ctx.locals;
  return prog;
}

// -----------------------------------------------------------------------------
// AST printing
// -----------------------------------------------------------------------------

static void ast_print_expr(Node *node, uint8_t indent) {
  if (node->kind != ND_EXPR) {
    ERROR_NODE_KIND(node);
    return;
  }
  for (uint8_t i = 0; i < indent; ++i) {
    fprintf(stderr, "  ");
  }
  switch (node->tag.expr) {
  case EXPR_VAR:
    fprintf(stderr, "%s: '%.*s'\n", EXPR_KIND_STR[node->tag.expr],
            (int)node->lex.len, node->lex.ptr);
    return;
  case EXPR_NUM:
    fprintf(stderr, "%s: %d\n", EXPR_KIND_STR[node->tag.expr], node->val.i);
    return;
  case EXPR_NEG:
    fprintf(stderr, "%s:\n", EXPR_KIND_STR[node->tag.expr]);
    ast_print_expr(node->lhs, indent + 1);
    return;
  case EXPR_ADD:
  case EXPR_SUB:
  case EXPR_MUL:
  case EXPR_DIV:
  case EXPR_EQ:
  case EXPR_NE:
  case EXPR_LT:
  case EXPR_LE:
  case EXPR_GT:
  case EXPR_GE:
  case EXPR_ASSIGN:
    fprintf(stderr, "%s:\n", EXPR_KIND_STR[node->tag.expr]);
    break;
  default:
    ERROR_NODE_KIND(node);
    break;
  }
  ast_print_expr(node->lhs, indent + 1);
  ast_print_expr(node->rhs, indent + 1);
}

static void ast_print_stmt(Node *node, uint8_t indent) {
  if (node->kind != ND_STMT) {
    ERROR_NODE_KIND(node);
    return;
  }
  for (uint8_t i = 0; i < indent; ++i) {
    fprintf(stderr, "  ");
  }
  switch (node->tag.stmt) {
  case STMT_BLOCK:
    fprintf(stderr, "%s:\n", STMT_KIND_STR[node->tag.stmt]);
    for (Node *n = node->lhs; n != NULL; n = n->next) {
      ast_print_stmt(n, indent + 1);
    }
    break;
  case STMT_EXPR:
  case STMT_RETURN:
    fprintf(stderr, "%s:\n", STMT_KIND_STR[node->tag.stmt]);
    ast_print_expr(node->lhs, indent + 1);
    break;
  default:
    ERROR_STMT_KIND(node);
    return;
  }
}

static void ast_print(Function *prog) {
  if (prog->kind == FK_ERROR) {
    ERROR_FUNCTION_KIND(prog);
    return;
  }
  for (Node *n = prog->body; n != NULL; n = n->next) {
    ast_print_stmt(n, 0);
  }
}

// -----------------------------------------------------------------------------
// Offset calculation
// -----------------------------------------------------------------------------

static int align_to(int n, int align) {
  return (n + align - 1) / align * align;
}

static void assign_local_offsets(Function *prog) {
  if (prog->kind == FK_ERROR) {
    ERROR_FUNCTION_KIND(prog);
    return;
  }
  int offset = 0;
  for (Object *o = prog->locals; o != NULL; o = o->next) {
    offset += 8;
    o->offset = -offset;
  }
  prog->stack_size = align_to(offset, 16);
}

// -----------------------------------------------------------------------------
// Code generation
// -----------------------------------------------------------------------------

static void gen_addr(Node *node) {
  if (node->kind != ND_EXPR) {
    ERROR_NODE_KIND(node);
    return;
  } else if (node->tag.expr != EXPR_VAR) {
    ERROR_EXPR_KIND(node);
    return;
  }
  printf("  lea %d(%%rbp), %%rax\n", node->val.obj->offset);
}

static void gen_expr(Node *node) {
  if (node->kind != ND_EXPR) {
    ERROR_NODE_KIND(node);
    return;
  }
  switch (node->tag.expr) {
  case EXPR_NUM:
    printf("  mov $%d, %%rax\n", node->val.i);
    return;
  case EXPR_VAR:
    gen_addr(node);
    printf("  mov (%%rax), %%rax\n");
    return;
  case EXPR_NEG:
    gen_expr(node->lhs);
    printf("  neg %%rax\n");
    return;
  case EXPR_ASSIGN:
    gen_addr(node->lhs);
    printf("  push %%rax\n");
    gen_expr(node->rhs);
    printf("  pop %%rdi\n");
    printf("  mov %%rax, (%%rdi)\n");
    return;
  default:
    break;
  }
  gen_expr(node->rhs);
  printf("  push %%rax\n");
  gen_expr(node->lhs);
  printf("  pop %%rdi\n");
  switch (node->tag.expr) {
  case EXPR_ADD:
    printf("  add %%rdi, %%rax\n");
    break;
  case EXPR_SUB:
    printf("  sub %%rdi, %%rax\n");
    break;
  case EXPR_MUL:
    printf("  imul %%rdi, %%rax\n");
    break;
  case EXPR_DIV:
    printf("  cqo\n");
    printf("  idiv %%rdi\n");
    break;
  case EXPR_EQ:
    printf("  cmp %%rdi, %%rax\n");
    printf("  sete %%al\n");
    printf("  movzb %%al, %%rax\n");
    break;
  case EXPR_NE:
    printf("  cmp %%rdi, %%rax\n");
    printf("  setne %%al\n");
    printf("  movzb %%al, %%rax\n");
    break;
  case EXPR_LT:
    printf("  cmp %%rdi, %%rax\n");
    printf("  setl %%al\n");
    printf("  movzb %%al, %%rax\n");
    break;
  case EXPR_LE:
    printf("  cmp %%rdi, %%rax\n");
    printf("  setle %%al\n");
    printf("  movzb %%al, %%rax\n");
    break;
  case EXPR_GT:
    printf("  cmp %%rdi, %%rax\n");
    printf("  setg %%al\n");
    printf("  movzb %%al, %%rax\n");
    break;
  case EXPR_GE:
    printf("  cmp %%rdi, %%rax\n");
    printf("  setge %%al\n");
    printf("  movzb %%al, %%rax\n");
    break;
  case EXPR_NUM:
  case EXPR_VAR:
  case EXPR_NEG:
  case EXPR_ASSIGN:
    ERROR_EXPR_KIND(node);
  }
}

static void gen_stmt(Node *node) {
  if (node->kind != ND_STMT) {
    ERROR_NODE_KIND(node);
    return;
  }
  switch (node->tag.stmt) {
  case STMT_EXPR:
    gen_expr(node->lhs);
    return;
  case STMT_RETURN:
    gen_expr(node->lhs);
    printf("  jmp .L.return\n");
    return;
  case STMT_BLOCK:
    for (Node *n = node->lhs; n != NULL; n = n->next) {
      gen_stmt(n);
    }
    break;
  default:
    ERROR_STMT_KIND(node);
    return;
  }
}

static void codegen(Function *prog) {
  assign_local_offsets(prog);
  DEBUGF("stack size: %d", prog->stack_size);
  printf("  .globl main\n");
  printf("main:\n");
  printf("  push %%rbp\n");
  printf("  mov %%rsp, %%rbp\n");
  printf("  sub $%d, %%rsp\n", prog->stack_size);
  for (Node *n = prog->body; n != NULL; n = n->next) {
    gen_stmt(n);
  }
  printf(".L.return:\n");
  printf("  mov %%rbp, %%rsp\n");
  printf("  pop %%rbp\n");
  printf("  ret\n");
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
  Lexer lexer = lexer_create(start);
  DEBUG("Tokens:");
  for (Token *t = lexer_peek(&lexer); t->kind != TK_EOF;
       t = lexer_next(&lexer)) {
    switch (lexer_peek(&lexer)->kind) {
    case TK_ERR:
    case TK_IDENT:
      fprintf(stderr, "%s: '%.*s'\n", TOKEN_KIND_STR[t->kind],
              (int)lexer_peek(&lexer)->lex.len, lexer_peek(&lexer)->lex.ptr);
      break;
    case TK_NUM:
      fprintf(stderr, "%s: %d\n", TOKEN_KIND_STR[t->kind],
              lexer_peek(&lexer)->val.i);
      break;
    default:
      fprintf(stderr, "%s\n", TOKEN_KIND_STR[t->kind]);
      break;
    }
  }

  lexer = lexer_create(start);
  Arena arena = arena_default();
  Function *prog = parse(&arena, &lexer);
  if (prog->kind == FK_ERROR) {
    return EXIT_FAILURE;
  }

  DEBUG("AST:");
  ast_print(prog);

  if (lexer_peek(&lexer)->kind != TK_EOF) {
    error_at(start, lexer.token.lex.ptr,
             "unexpected token during parsing: '%.*s'\n", lexer.token.lex.len,
             lexer.token.lex.ptr);
    return EXIT_FAILURE;
  }

  codegen(prog);

  DEBUGF("total blocks allocated: %u", arena_total_blocks_allocated(&arena));
  DEBUGF("current bytes committed: %zu", arena.commited_size);
  DEBUGF("total bytes allocated: %zu", arena_total_bytes_allocated(&arena));

  arena_destroy(&arena);

  return EXIT_SUCCESS;
}
