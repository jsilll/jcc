// -----------------------------------------------------------------------------
// Prelude
// -----------------------------------------------------------------------------

#include "arena.h"
#include "lexer.h"
#include "string_view.h"

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

// -----------------------------------------------------------------------------
// Error handling
// -----------------------------------------------------------------------------

static void error(const char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  vfprintf(stderr, fmt, ap);
  va_end(ap);
}

static void verror_at(const char *start, const char *loc, const char *fmt,
                      va_list ap) {
  int pos = loc - start;
  fprintf(stderr, "%s\n", start);
  fprintf(stderr, "%*s", pos, "");
  fprintf(stderr, "^ ");
  vfprintf(stderr, fmt, ap);
  fprintf(stderr, "\n");
}

static void error_at(const char *start, const char *loc, const char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  verror_at(start, loc, fmt, ap);
}

// -----------------------------------------------------------------------------
// Ast
// -----------------------------------------------------------------------------

#include "ast.h"

static Object *object_create(Arena *a, Object *all, StringView lex) {
  Object *o = arena_alloc(a, sizeof(Object));
  o->lex = lex;
  o->next = all;
  o->offset = 0;
  return o;
}

static ExprNode *expr_create(Arena *a, ExprKind kind) {
  ExprNode *node = arena_alloc(a, sizeof(ExprNode));
  node->kind = kind;
  return node;
}

static ExprNode *expr_create_err(Arena *a, Token *t) {
  ExprNode *node = expr_create(a, EXPR_ERR);
  node->lex = t->lex;
  return node;
}

static ExprNode *expr_create_num(Arena *a, Token *t) {
  ExprNode *node = expr_create(a, EXPR_NUM);
  node->u.num = t->u.num;
  node->lex = t->lex;
  return node;
}

static ExprNode *expr_create_var(Arena *a, Object *o) {
  ExprNode *node = expr_create(a, EXPR_VAR);
  node->lex = o->lex;
  node->u.var = o;
  return node;
}

static ExprNode *expr_create_unary(Arena *a, Token *t, UnOpKind op,
                                   ExprNode *expr) {
  ExprNode *node = expr_create(a, EXPR_UN);
  node->u.unary.expr = expr;
  node->u.unary.op = op;
  node->lex = t->lex;
  return node;
}

static ExprNode *expr_create_binary(Arena *a, Token *t, BinOpKind op,
                                    ExprNode *lhs, ExprNode *rhs) {
  ExprNode *node = expr_create(a, EXPR_BIN);
  node->u.binary.lhs = lhs;
  node->u.binary.rhs = rhs;
  node->u.binary.op = op;
  node->lex = t->lex;
  return node;
}

static StmtNode *stmt_create(Arena *a, StmtKind kind) {
  StmtNode *node = arena_alloc(a, sizeof(StmtNode));
  node->kind = kind;
  return node;
}

static StmtNode *stmt_create_err(Arena *a, Token *t) {
  StmtNode *node = stmt_create(a, STMT_ERR);
  node->lex = t->lex;
  return node;
}

static StmtNode *stmt_create_ret(Arena *a, Token *t, ExprNode *expr) {
  StmtNode *node = stmt_create(a, STMT_RETURN);
  node->u.ret.expr = expr;
  node->lex = t->lex;
  return node;
}

static StmtNode *stmt_create_expr(Arena *a, Token *t, ExprNode *expr) {
  StmtNode *node = stmt_create(a, STMT_EXPR);
  node->u.expr.expr = expr;
  node->lex = t->lex;
  return node;
}

static StmtNode *stmt_create_block(Arena *a, Token *t, StmtNode *body) {
  StmtNode *node = stmt_create(a, STMT_BLOCK);
  node->u.block.stmt = body;
  node->lex = t->lex;
  return node;
}

static StmtNode *stmt_create_while(Arena *a, Token *t, ExprNode *cond,
                                   StmtNode *body) {
  StmtNode *node = stmt_create(a, STMT_WHILE);
  node->u.whil.cond = cond;
  node->u.whil.then = body;
  node->lex = t->lex;
  return node;
}

static StmtNode *stmt_create_if(Arena *a, Token *t, ExprNode *cond,
                                StmtNode *then, StmtNode *els) {
  StmtNode *node = stmt_create(a, STMT_IF);
  node->u.iff.cond = cond;
  node->u.iff.then = then;
  node->u.iff.els = els;
  node->lex = t->lex;
  return node;
}

static StmtNode *stmt_create_for(Arena *a, Token *t, StmtNode *init,
                                 ExprNode *cond, ExprNode *step,
                                 StmtNode *body) {
  StmtNode *node = stmt_create(a, STMT_FOR);
  node->u.forr.init = init;
  node->u.forr.cond = cond;
  node->u.forr.step = step;
  node->u.forr.then = body;
  node->lex = t->lex;
  return node;
}

// -----------------------------------------------------------------------------
// Parser
// -----------------------------------------------------------------------------

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
  BinOpKind op;
  BindingPower power;
} BinOpPair;

#define PREC_MIN 1
#define PREC_MAX 12

static BinOpPair try_into_binop(TokenKind kind) {
  switch (kind) {
  case TK_EQ:
    return (BinOpPair){BINOP_ASGN, (BindingPower){3, 2}};
  case TK_EQEQ:
    return (BinOpPair){BINOP_EQ, (BindingPower){4, 5}};
  case TK_NE:
    return (BinOpPair){BINOP_NE, (BindingPower){4, 5}};
  case TK_LT:
    return (BinOpPair){BINOP_LT, (BindingPower){6, 7}};
  case TK_LE:
    return (BinOpPair){BINOP_LE, (BindingPower){6, 7}};
  case TK_GT:
    return (BinOpPair){BINOP_GT, (BindingPower){6, 7}};
  case TK_GE:
    return (BinOpPair){BINOP_GE, (BindingPower){6, 7}};
  case TK_PLUS:
    return (BinOpPair){BINOP_ADD, (BindingPower){8, 9}};
  case TK_MINUS:
    return (BinOpPair){BINOP_SUB, (BindingPower){8, 9}};
  case TK_MUL:
    return (BinOpPair){BINOP_MUL, (BindingPower){10, 11}};
  case TK_DIV:
    return (BinOpPair){BINOP_DIV, (BindingPower){10, 11}};
  default:
    return (BinOpPair){BINOP_ADD, (BindingPower){0, 0}};
  }
}

typedef struct {
  Arena *arena;
  Lexer *lexer;
  Object *locals;
} ParseCtx;

static Object *parse_ctx_find_local(ParseCtx *ctx, StringView lex) {
  for (Object *o = ctx->locals; o != NULL; o = o->next) {
    if (string_view_equals(o->lex, lex)) {
      return o;
    }
  }
  return NULL;
}

static ExprNode *parse_expr(ParseCtx *ctx, uint8_t prec);

static ExprNode *parse_expr_primary(ParseCtx *ctx) {
  Token *t = lexer_next(ctx->lexer);
  switch (t->kind) {
  case TK_NUM:
    return expr_create_num(ctx->arena, t);
  case TK_IDENT: {
    Object *obj = parse_ctx_find_local(ctx, t->lex);
    if (obj != NULL) {
      return expr_create_var(ctx->arena, obj);
    }
    ctx->locals = object_create(ctx->arena, ctx->locals, t->lex);
    return expr_create_var(ctx->arena, ctx->locals);
  }
  case TK_PLUS:
    return parse_expr_primary(ctx);
  case TK_MINUS:
    return expr_create_unary(ctx->arena, t, UNOP_NEG,
                             parse_expr(ctx, PREC_MAX));
  case TK_LPAREN: {
    ExprNode *node = parse_expr(ctx, 0);
    if (lexer_peek(ctx->lexer)->kind != TK_RPAREN) {
      return expr_create_err(ctx->arena, lexer_peek(ctx->lexer));
    }
    lexer_next(ctx->lexer);
    return node;
  }
  default:
    return expr_create_err(ctx->arena, t);
  }
}

static ExprNode *parse_expr(ParseCtx *ctx, uint8_t prec) {
  ExprNode *lhs = parse_expr_primary(ctx);
  while (true) {
    Token t = *lexer_peek(ctx->lexer);
    BinOpPair bp = try_into_binop(t.kind);
    if (bp.power.lhs <= prec) {
      return lhs;
    }
    lexer_next(ctx->lexer);
    lhs = expr_create_binary(ctx->arena, &t, bp.op, lhs,
                             parse_expr(ctx, bp.power.rhs));
  }
  return lhs;
}

static StmtNode *parse_stmt(ParseCtx *ctx);

static StmtNode *parse_stmt_block(ParseCtx *ctx) {
  Token t = *lexer_next(ctx->lexer);
  StmtNode head = {0};
  StmtNode *cur = &head;
  while (lexer_peek(ctx->lexer)->kind != TK_RBRACE) {
    cur->next = parse_stmt(ctx);
    cur = cur->next;
  }
  lexer_next(ctx->lexer);
  return stmt_create_block(ctx->arena, &t, head.next);
}

static StmtNode *parse_stmt(ParseCtx *ctx) {
  switch (lexer_peek(ctx->lexer)->kind) {
  case TK_SEMI:
    return stmt_create_block(ctx->arena, lexer_next(ctx->lexer), NULL);
  case TK_KW_RETURN: {
    lexer_next(ctx->lexer);
    ExprNode *node = parse_expr(ctx, PREC_MIN);
    if (lexer_peek(ctx->lexer)->kind != TK_SEMI) {
      return stmt_create_err(ctx->arena, lexer_peek(ctx->lexer));
    }
    return stmt_create_ret(ctx->arena, lexer_next(ctx->lexer), node);
  }
  case TK_LBRACE:
    return parse_stmt_block(ctx);
  case TK_KW_IF: {
    Token t = *lexer_next(ctx->lexer);
    if (lexer_peek(ctx->lexer)->kind != TK_LPAREN) {
      return stmt_create_err(ctx->arena, lexer_peek(ctx->lexer));
    }
    lexer_next(ctx->lexer);
    ExprNode *cond = parse_expr(ctx, PREC_MIN);
    if (lexer_peek(ctx->lexer)->kind != TK_RPAREN) {
      return stmt_create_err(ctx->arena, lexer_peek(ctx->lexer));
    }
    lexer_next(ctx->lexer);
    StmtNode *body = parse_stmt(ctx);
    if (lexer_peek(ctx->lexer)->kind == TK_KW_ELSE) {
      lexer_next(ctx->lexer);
      StmtNode *els3 = parse_stmt(ctx);
      return stmt_create_if(ctx->arena, &t, cond, body, els3);
    }
    return stmt_create_if(ctx->arena, &t, cond, body, NULL);
  }
  case TK_KW_FOR: {
    Token t = *lexer_next(ctx->lexer);
    if (lexer_peek(ctx->lexer)->kind != TK_LPAREN) {
      return stmt_create_err(ctx->arena, lexer_peek(ctx->lexer));
    }
    lexer_next(ctx->lexer);
    StmtNode *init = parse_stmt(ctx);
    ExprNode *cond = NULL;
    if (lexer_peek(ctx->lexer)->kind != TK_SEMI) {
      cond = parse_expr(ctx, PREC_MIN);
    }
    if (lexer_peek(ctx->lexer)->kind != TK_SEMI) {
      return stmt_create_err(ctx->arena, lexer_peek(ctx->lexer));
    }
    lexer_next(ctx->lexer);
    ExprNode *step = NULL;
    if (lexer_peek(ctx->lexer)->kind != TK_RPAREN) {
      step = parse_expr(ctx, PREC_MIN);
    }
    if (lexer_peek(ctx->lexer)->kind != TK_RPAREN) {
      return stmt_create_err(ctx->arena, lexer_peek(ctx->lexer));
    }
    lexer_next(ctx->lexer);
    StmtNode *body = parse_stmt(ctx);
    return stmt_create_for(ctx->arena, &t, init, cond, step, body);
  }
  case TK_KW_WHILE: {
    Token t = *lexer_next(ctx->lexer);
    if (lexer_peek(ctx->lexer)->kind != TK_LPAREN) {
      return stmt_create_err(ctx->arena, lexer_peek(ctx->lexer));
    }
    lexer_next(ctx->lexer);
    ExprNode *cond = parse_expr(ctx, PREC_MIN);
    if (lexer_peek(ctx->lexer)->kind != TK_RPAREN) {
      return stmt_create_err(ctx->arena, lexer_peek(ctx->lexer));
    }
    lexer_next(ctx->lexer);
    StmtNode *body = parse_stmt(ctx);
    return stmt_create_while(ctx->arena, &t, cond, body);
  }
  default: {
    ExprNode *node = parse_expr(ctx, PREC_MIN);
    if (lexer_peek(ctx->lexer)->kind != TK_SEMI) {
      return stmt_create_err(ctx->arena, lexer_peek(ctx->lexer));
    }
    return stmt_create_expr(ctx->arena, lexer_next(ctx->lexer), node);
  }
  }
}

static FuncNode parse(Arena *a, Lexer *l) {
  if (lexer_peek(l)->kind != TK_LBRACE) {
    error("unexpected token during parsing: '%.*s'\n", lexer_peek(l)->lex.len,
          lexer_peek(l)->lex.ptr);
    return (FuncNode){0};
  }
  ParseCtx ctx = {a, l, NULL};
  FuncNode prog = {0};
  prog.body = parse_stmt_block(&ctx);
  prog.locals = ctx.locals;
  return prog;
}

// -----------------------------------------------------------------------------
// AST printing
// -----------------------------------------------------------------------------

static void ast_print_expr(ExprNode *node, uint8_t indent) {
  for (uint8_t i = 0; i < indent; ++i) {
    fprintf(stderr, "  ");
  }
  switch (node->kind) {
  case EXPR_VAR:
    fprintf(stderr, "%s: '%.*s'\n", EXPR_KIND_STR[node->kind],
            (int)node->lex.len, node->lex.ptr);
    break;
  case EXPR_NUM:
    fprintf(stderr, "%s: %d\n", EXPR_KIND_STR[node->kind], node->u.num);
    break;
  case EXPR_UN:
    fprintf(stderr, "%s:\n", UNOP_KIND_STR[node->u.unary.op]);
    ast_print_expr(node->u.unary.expr, indent + 1);
    break;
  case EXPR_BIN:
    fprintf(stderr, "%s:\n", BINOP_KIND_STR[node->u.binary.op]);
    ast_print_expr(node->u.binary.lhs, indent + 1);
    ast_print_expr(node->u.binary.rhs, indent + 1);
    break;
  case EXPR_ERR:
    ERROR_EXPR_KIND(node);
    break;
  }
}

static void ast_print_stmt(StmtNode *node, uint8_t indent) {
  for (uint8_t i = 0; i < indent; ++i) {
    fprintf(stderr, "  ");
  }
  switch (node->kind) {
  case STMT_RETURN:
    fprintf(stderr, "%s:\n", STMT_KIND_STR[node->kind]);
    ast_print_expr(node->u.ret.expr, indent + 1);
    break;
  case STMT_EXPR:
    fprintf(stderr, "%s:\n", STMT_KIND_STR[node->kind]);
    ast_print_expr(node->u.expr.expr, indent + 1);
    break;
  case STMT_BLOCK:
    fprintf(stderr, "%s:\n", STMT_KIND_STR[node->kind]);
    for (StmtNode *n = node->u.block.stmt; n != NULL; n = n->next) {
      ast_print_stmt(n, indent + 1);
    }
    break;
  case STMT_WHILE:
    fprintf(stderr, "%s:\n", STMT_KIND_STR[node->kind]);
    ast_print_expr(node->u.whil.cond, indent + 1);
    ast_print_stmt(node->u.whil.then, indent + 1);
    break;
  case STMT_IF:
    fprintf(stderr, "%s:\n", STMT_KIND_STR[node->kind]);
    ast_print_expr(node->u.iff.cond, indent + 1);
    ast_print_stmt(node->u.iff.then, indent + 1);
    if (node->u.iff.els != NULL) {
      ast_print_stmt(node->u.iff.els, indent + 1);
    }
    break;
  case STMT_FOR:
    fprintf(stderr, "%s:\n", STMT_KIND_STR[node->kind]);
    ast_print_stmt(node->u.forr.init, indent + 1);
    if (node->u.forr.cond != NULL) {
      ast_print_expr(node->u.forr.cond, indent + 1);
    }
    if (node->u.forr.step != NULL) {
      ast_print_expr(node->u.forr.step, indent + 1);
    }
    ast_print_stmt(node->u.forr.then, indent + 1);
    break;
  case STMT_ERR:
    ERROR_STMT_KIND(node);
    break;
  }
}

static void ast_print(FuncNode *prog) {
  for (StmtNode *n = prog->body; n != NULL; n = n->next) {
    ast_print_stmt(n, 0);
  }
}

// -----------------------------------------------------------------------------
// Offset calculation
// -----------------------------------------------------------------------------

static int align_to(int n, int align) {
  return (n + align - 1) / align * align;
}

static void assign_local_offsets(FuncNode *prog) {
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

static uint32_t gen_uid(void) {
  static uint32_t uid = 1;
  return uid++;
}

static void gen_addr(ExprNode *node) {
  if (node->kind != EXPR_VAR) {
    ERROR_EXPR_KIND(node);
    return;
  }
  printf("  lea %d(%%rbp), %%rax\n", node->u.var->offset);
}

static void gen_expr(ExprNode *node) {
  switch (node->kind) {
  case EXPR_NUM:
    printf("  mov $%d, %%rax\n", node->u.num);
    break;
  case EXPR_VAR:
    gen_addr(node);
    printf("  mov (%%rax), %%rax\n");
    break;
  case EXPR_UN:
    switch (node->u.unary.op) {
    case UNOP_NEG:
      gen_expr(node->u.unary.expr);
      printf("  neg %%rax\n");
      break;
    default:
      ERROR_UNOP_KIND(node);
    }
    break;
  case EXPR_BIN:
    if (node->u.binary.op == BINOP_ASGN) {
      gen_addr(node->u.binary.lhs);
      printf("  push %%rax\n");
      gen_expr(node->u.binary.rhs);
      printf("  pop %%rdi\n");
      printf("  mov %%rax, (%%rdi)\n");
      return;
    }
    gen_expr(node->u.binary.rhs);
    printf("  push %%rax\n");
    gen_expr(node->u.binary.lhs);
    printf("  pop %%rdi\n");
    switch (node->u.binary.op) {
    case BINOP_ADD:
      printf("  add %%rdi, %%rax\n");
      break;
    case BINOP_SUB:
      printf("  sub %%rdi, %%rax\n");
      break;
    case BINOP_MUL:
      printf("  imul %%rdi, %%rax\n");
      break;
    case BINOP_DIV:
      printf("  cqo\n");
      printf("  idiv %%rdi\n");
      break;
    case BINOP_EQ:
      printf("  cmp %%rdi, %%rax\n");
      printf("  sete %%al\n");
      printf("  movzb %%al, %%rax\n");
      break;
    case BINOP_NE:
      printf("  cmp %%rdi, %%rax\n");
      printf("  setne %%al\n");
      printf("  movzb %%al, %%rax\n");
      break;
    case BINOP_LT:
      printf("  cmp %%rdi, %%rax\n");
      printf("  setl %%al\n");
      printf("  movzb %%al, %%rax\n");
      break;
    case BINOP_LE:
      printf("  cmp %%rdi, %%rax\n");
      printf("  setle %%al\n");
      printf("  movzb %%al, %%rax\n");
      break;
    case BINOP_GT:
      printf("  cmp %%rdi, %%rax\n");
      printf("  setg %%al\n");
      printf("  movzb %%al, %%rax\n");
      break;
    case BINOP_GE:
      printf("  cmp %%rdi, %%rax\n");
      printf("  setge %%al\n");
      printf("  movzb %%al, %%rax\n");
      break;
    default:
      ERROR_BINOP_KIND(node);
    }
    break;
  case EXPR_ERR:
    ERROR_EXPR_KIND(node);
    break;
  }
}

static void gen_stmt(StmtNode *node) {
  switch (node->kind) {
  case STMT_EXPR:
    gen_expr(node->u.expr.expr);
    break;
  case STMT_RETURN:
    gen_expr(node->u.ret.expr);
    printf("  jmp .L.return\n");
    break;
  case STMT_BLOCK:
    for (StmtNode *n = node->u.block.stmt; n != NULL; n = n->next) {
      gen_stmt(n);
    }
    break;
  case STMT_IF: {
    uint32_t uid = gen_uid();
    gen_expr(node->u.iff.cond);
    printf("  test %%rax, %%rax\n");
    printf("  je .L.else.%u\n", uid);
    gen_stmt(node->u.iff.then);
    printf("  jmp .L.end.%u\n", uid);
    printf(".L.else.%u:\n", uid);
    if (node->u.iff.els != NULL) {
      gen_stmt(node->u.iff.els);
    }
    printf(".L.end.%u:\n", uid);
    break;
  }
  case STMT_WHILE: {
    uint32_t c = gen_uid();
    printf(".L.begin.%d:\n", c);
    gen_expr(node->u.whil.cond);
    printf("  test %%rax, %%rax\n");
    printf("  je  .L.end.%d\n", c);
    gen_stmt(node->u.whil.then);
    printf("  jmp .L.begin.%d\n", c);
    printf(".L.end.%d:\n", c);
    break;
  }
  case STMT_FOR: {
    uint32_t c = gen_uid();
    gen_stmt(node->u.forr.init);
    printf(".L.begin.%d:\n", c);
    if (node->u.forr.cond != NULL) {
      gen_expr(node->u.forr.cond);
      printf("  test %%rax, %%rax\n");
      printf("  je  .L.end.%d\n", c);
    }
    gen_stmt(node->u.forr.then);
    if (node->u.forr.step != NULL) {
      gen_expr(node->u.forr.step);
    }
    printf("  jmp .L.begin.%d\n", c);
    printf(".L.end.%d:\n", c);
    break;
  }
  case STMT_ERR:
    ERROR_STMT_KIND(node);
    break;
  }
}

static void codegen(FuncNode *prog) {
  assign_local_offsets(prog);
  DEBUGF("stack size: %d", prog->stack_size);
  printf("  .globl main\n");
  printf("main:\n");
  printf("  push %%rbp\n");
  printf("  mov %%rsp, %%rbp\n");
  printf("  sub $%d, %%rsp\n", prog->stack_size);
  for (StmtNode *n = prog->body; n != NULL; n = n->next) {
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
              lexer_peek(&lexer)->u.num);
      break;
    default:
      fprintf(stderr, "%s\n", TOKEN_KIND_STR[t->kind]);
    }
  }

  lexer = lexer_create(start);
  Arena arena = arena_default();
  FuncNode prog = parse(&arena, &lexer);
  if (prog.body == NULL) {
    error("failed to parse program\n");
    return EXIT_FAILURE;
  }

  DEBUG("AST:");
  ast_print(&prog);

  if (lexer_peek(&lexer)->kind != TK_EOF) {
    error_at(start, lexer.token.lex.ptr,
             "unexpected token during parsing: '%.*s'\n", lexer.token.lex.len,
             lexer.token.lex.ptr);
    return EXIT_FAILURE;
  }

  codegen(&prog);

  DEBUGF("total blocks allocated: %u", arena_total_blocks_allocated(&arena));
  DEBUGF("current bytes committed: %zu", arena.commited_size);
  DEBUGF("total bytes allocated: %zu", arena_total_bytes_allocated(&arena));

  arena_destroy(&arena);

  return EXIT_SUCCESS;
}
