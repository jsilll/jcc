// -----------------------------------------------------------------------------
// Prelude
// -----------------------------------------------------------------------------

#include "arena.h"
#include "ast.h"
#include "error.h"
#include "hash_map.h"
#include "hash_set.h"
#include "lexer.h"
#include "string_view.h"
#include "type_check.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// -----------------------------------------------------------------------------
// Ast
// -----------------------------------------------------------------------------

static Object *object_create(Arena *a, StringView lex, Object *all) {
  Object *o = arena_alloc(a, sizeof(Object));
  o->lex = lex;
  o->next = all;
  o->offset = 0;
  return o;
}

static ExprNode *expr_create(Arena *a, StringView lex, ExprKind kind) {
  ExprNode *node = arena_alloc(a, sizeof(ExprNode));
  node->kind = kind;
  node->type = NULL;
  node->lex = lex;
  return node;
}

static ExprNode *expr_create_err(Arena *a, StringView lex) {
  ExprNode *node = expr_create(a, lex, EXPR_ERR);
  return node;
}

static ExprNode *expr_create_num(Arena *a, Token *token) {
  ExprNode *node = expr_create(a, token->lex, EXPR_NUM);
  node->u.num = token->u.num;
  return node;
}

static ExprNode *expr_create_var(Arena *a, StringView lex, Object *obj) {
  ExprNode *node = expr_create(a, lex, EXPR_VAR);
  node->u.var.obj = obj;
  node->u.var.id = 0;
  return node;
}

static ExprNode *expr_create_unary(Arena *a, StringView lex, UnOpKind op,
                                   ExprNode *expr) {
  ExprNode *node = expr_create(a, lex, EXPR_UN);
  node->u.unary.expr = expr;
  node->u.unary.op = op;
  return node;
}

static ExprNode *expr_create_binary(Arena *a, StringView lex, BinOpKind op,
                                    ExprNode *lhs, ExprNode *rhs) {
  ExprNode *node = expr_create(a, lex, EXPR_BIN);
  node->u.binary.lhs = lhs;
  node->u.binary.rhs = rhs;
  node->u.binary.op = op;
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
  case TK_STAR:
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
      return expr_create_var(ctx->arena, t->lex, obj);
    }
    ctx->locals = object_create(ctx->arena, t->lex, ctx->locals);
    return expr_create_var(ctx->arena, t->lex, ctx->locals);
  }
  case TK_PLUS:
    return parse_expr_primary(ctx);
  case TK_MINUS:
    return expr_create_unary(ctx->arena, t->lex, UNOP_NEG,
                             parse_expr(ctx, PREC_MAX));
  case TK_AMP:
    return expr_create_unary(ctx->arena, t->lex, UNOP_ADDR,
                             parse_expr(ctx, PREC_MAX));
  case TK_STAR:
    return expr_create_unary(ctx->arena, t->lex, UNOP_DEREF,
                             parse_expr(ctx, PREC_MAX));
  case TK_LPAREN: {
    ExprNode *node = parse_expr(ctx, 0);
    if (lexer_peek(ctx->lexer)->kind != TK_RPAREN) {
      return expr_create_err(ctx->arena, lexer_peek(ctx->lexer)->lex);
    }
    lexer_next(ctx->lexer);
    return node;
  }
  default:
    return expr_create_err(ctx->arena, t->lex);
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
    lhs = expr_create_binary(ctx->arena, t.lex, bp.op, lhs,
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
    if (node->type != NULL) {
      fprintf(stderr, "%s<%s>: %ld ('%.*s')\n", EXPR_KIND_STR[node->kind],
              TYPE_KIND_STR[node->type->kind], node->u.var.id,
              (int)node->lex.len, node->lex.ptr);
    } else {
      fprintf(stderr, "%s: %ld ('%.*s')\n", EXPR_KIND_STR[node->kind],
              node->u.var.id, (int)node->lex.len, node->lex.ptr);
    }
    break;
  case EXPR_NUM:
    if (node->type != NULL) {
      fprintf(stderr, "%s<%s>: %d\n", EXPR_KIND_STR[node->kind],
              TYPE_KIND_STR[node->type->kind], node->u.num);
    } else {
      fprintf(stderr, "%s: %d\n", EXPR_KIND_STR[node->kind], node->u.num);
    }
    break;
  case EXPR_UN:
    if (node->type != NULL) {
      fprintf(stderr, "%s<%s>:\n", UNOP_KIND_STR[node->u.unary.op],
              TYPE_KIND_STR[node->type->kind]);
    } else {
      fprintf(stderr, "%s:\n", UNOP_KIND_STR[node->u.unary.op]);
    }
    ast_print_expr(node->u.unary.expr, indent + 1);
    break;
  case EXPR_BIN:
    if (node->type != NULL) {
      fprintf(stderr, "%s<%s>:\n", BINOP_KIND_STR[node->u.binary.op],
              TYPE_KIND_STR[node->type->kind]);
    } else {
      fprintf(stderr, "%s:\n", BINOP_KIND_STR[node->u.binary.op]);
    }
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
// Name Resolution
// -----------------------------------------------------------------------------

uint64_t hash_string_view(const void *key) {
  StringView *sv = (StringView *)key;
  uint64_t hash = 0xcbf29ce484222325;
  for (size_t i = 0; i < sv->len; ++i) {
    hash ^= sv->ptr[i];
    hash *= 0x100000001b3;
  }
  return hash;
}

bool equal_string_view(const void *lhs, const void *rhs) {
  StringView *l = (StringView *)lhs;
  StringView *r = (StringView *)rhs;
  return l->len == r->len && memcmp(l->ptr, r->ptr, l->len) == 0;
}

typedef struct ResolveCtx {
  uint64_t id;
  HashMap locals;
} ResolveCtx;

ResolveCtx resolve_ctx_create() {
  ResolveCtx ctx = {
      .id = 1,
      .locals = hash_map_create(64, hash_string_view, equal_string_view)};
  return ctx;
}

void resolve_ctx_destroy(ResolveCtx *ctx) { hash_map_destroy(&ctx->locals); }

void resolve_expr(ResolveCtx *ctx, ExprNode *expr) {
  uint64_t id = 0;
  switch (expr->kind) {
  case EXPR_NUM:
    break;
  case EXPR_VAR:
    id = (uint64_t)hash_map_try_set(&ctx->locals, &expr->u.var.obj->lex,
                                    (void *)ctx->id);
    if (id != 0) {
      expr->u.var.id = id;
    } else {
      expr->u.var.id = ctx->id++;
    }
    break;
  case EXPR_UN:
    resolve_expr(ctx, expr->u.unary.expr);
    break;
  case EXPR_BIN:
    resolve_expr(ctx, expr->u.binary.lhs);
    resolve_expr(ctx, expr->u.binary.rhs);
    break;
  case EXPR_ERR:
    ERROR_EXPR_KIND(expr);
    break;
  }
}

void resolve_stmt(ResolveCtx *ctx, StmtNode *stmt) {
  switch (stmt->kind) {
  case STMT_RETURN:
    resolve_expr(ctx, stmt->u.ret.expr);
    break;
  case STMT_EXPR:
    resolve_expr(ctx, stmt->u.expr.expr);
    break;
  case STMT_BLOCK:
    for (StmtNode *n = stmt->u.block.stmt; n != NULL; n = n->next) {
      resolve_stmt(ctx, n);
    }
    break;
  case STMT_WHILE:
    resolve_expr(ctx, stmt->u.whil.cond);
    resolve_stmt(ctx, stmt->u.whil.then);
    break;
  case STMT_IF:
    resolve_expr(ctx, stmt->u.iff.cond);
    resolve_stmt(ctx, stmt->u.iff.then);
    if (stmt->u.iff.els != NULL) {
      resolve_stmt(ctx, stmt->u.iff.els);
    }
    break;
  case STMT_FOR:
    if (stmt->u.forr.init != NULL) {
      resolve_stmt(ctx, stmt->u.forr.init);
    }
    if (stmt->u.forr.cond != NULL) {
      resolve_expr(ctx, stmt->u.forr.cond);
    }
    if (stmt->u.forr.step != NULL) {
      resolve_expr(ctx, stmt->u.forr.step);
    }
    resolve_stmt(ctx, stmt->u.forr.then);
    break;
  case STMT_ERR:
    ERROR_STMT_KIND(stmt);
    break;
  }
}

void resolve(FuncNode *prog) {
  ResolveCtx ctx = resolve_ctx_create();
  for (StmtNode *stmt = prog->body; stmt != NULL; stmt = stmt->next) {
    resolve_stmt(&ctx, stmt);
  }
  resolve_ctx_destroy(&ctx);
}

// -----------------------------------------------------------------------------
// Type checking
// -----------------------------------------------------------------------------

static uint64_t hash_type(const void *key) {
  Type *t = (Type *)key;
  switch (t->kind) {
  case TYPE_VOID:
  case TYPE_INT:
    error("unexpected type kind during hashing: %d\n", t->kind);
    break;
  case TYPE_PTR:
    return (uint64_t)t->kind ^ (uint64_t)t->u.ptr.base;
  }
  return 0;
}

static bool equal_type(const void *lhs, const void *rhs) {
  Type *l = (Type *)lhs;
  Type *r = (Type *)rhs;
  if (l->kind != r->kind) {
    return false;
  }
  switch (l->kind) {
  case TYPE_VOID:
  case TYPE_INT:
    return true;
  case TYPE_PTR:
    return l->u.ptr.base == r->u.ptr.base;
  }
  return false;
}

typedef struct TypeCtx {
  Arena *arena;
  HashSet types;
} TypeCtx;

TypeCtx type_ctx_create(Arena *a) {
  TypeCtx ctx = {.arena = a,
                 .types = hash_set_create(64, hash_type, equal_type)};
  return ctx;
}

Type *type_ctx_make_type(TypeCtx *ctx, Type *type) {
  Type *t = arena_alloc(ctx->arena, sizeof(Type));
  *t = *type;
  t = (Type *)hash_set_insert(&ctx->types, t);
  if (t != type) {
    arena_undo(ctx->arena, sizeof(Type));
  }
  return t;
}

void type_ctx_destroy(TypeCtx *ctx) { hash_set_destroy(&ctx->types); }

static void type_check_expr(TypeCtx *ctx, ExprNode *node) {
  switch (node->kind) {
  case EXPR_NUM:
    node->type = TYPE_PRIM_INT;
    break;
  case EXPR_VAR:
    node->type = TYPE_PRIM_INT;
    break;
  case EXPR_UN:
    type_check_expr(ctx, node->u.unary.expr);
    switch (node->u.unary.op) {
    case UNOP_ADDR: {
      Type type =
          (Type){.kind = TYPE_PTR, .u.ptr.base = node->u.unary.expr->type};
      node->type = type_ctx_make_type(ctx, &type);
    } break;
    case UNOP_DEREF:
      if (node->u.unary.expr->type->kind != TYPE_PTR) {
        ERROR_TYPE_KIND(node->u.unary.expr);
      }
      node->type = node->u.unary.expr->type->u.ptr.base;
      break;
    default:
      node->type = node->u.unary.expr->type;
    }
    break;
  case EXPR_BIN:
    type_check_expr(ctx, node->u.binary.lhs);
    type_check_expr(ctx, node->u.binary.rhs);
    switch (node->u.binary.op) {
    case BINOP_EQ:
    case BINOP_NE:
    case BINOP_LT:
    case BINOP_LE:
    case BINOP_GT:
    case BINOP_GE:
      if (node->u.binary.lhs->type != node->u.binary.rhs->type) {
        ERROR_TYPE_KIND(node->u.binary.lhs);
      }
      node->type = TYPE_PRIM_INT;
      break;
    case BINOP_ASGN:
      node->u.binary.lhs->type = node->u.binary.rhs->type;
      node->type = node->u.binary.rhs->type;
      break;
    case BINOP_ADD:
      if (node->u.binary.lhs->type->kind == TYPE_PTR) {
        if (node->u.binary.rhs->type->kind != TYPE_INT) {
          ERROR_TYPE_KIND(node->u.binary.rhs);
        }
        node->type = node->u.binary.lhs->type;
      } else if (node->u.binary.rhs->type->kind == TYPE_PTR) {
        if (node->u.binary.lhs->type->kind != TYPE_INT) {
          ERROR_TYPE_KIND(node->u.binary.lhs);
        }
        node->type = node->u.binary.rhs->type;
      } else if (node->u.binary.lhs->type->kind == TYPE_INT &&
                 node->u.binary.rhs->type->kind == TYPE_INT) {
        node->type = TYPE_PRIM_INT;
      } else {
        ERROR_TYPE_KIND(node->u.binary.lhs);
      }
      break;
    case BINOP_SUB:
      if (node->u.binary.lhs->type->kind == TYPE_PTR) {
        if (node->u.binary.rhs->type->kind == TYPE_PTR) {
          if (node->u.binary.lhs->type->u.ptr.base !=
              node->u.binary.rhs->type->u.ptr.base) {
            ERROR_TYPE_KIND(node->u.binary.rhs);
          }
          node->type = TYPE_PRIM_INT;
        } else if (node->u.binary.rhs->type->kind == TYPE_INT) {
          node->type = node->u.binary.lhs->type;
        } else {
          ERROR_TYPE_KIND(node->u.binary.rhs);
        }
      } else if (node->u.binary.lhs->type->kind == TYPE_INT &&
                 node->u.binary.rhs->type->kind == TYPE_INT) {
        node->type = TYPE_PRIM_INT;
      } else {
        ERROR_TYPE_KIND(node->u.binary.lhs);
      }
      break;
    case BINOP_MUL:
      if (node->u.binary.lhs->type->kind != TYPE_INT ||
          node->u.binary.rhs->type->kind != TYPE_INT) {
        ERROR_TYPE_KIND(node->u.binary.lhs);
      }
      node->type = TYPE_PRIM_INT;
      break;
    case BINOP_DIV:
      if (node->u.binary.lhs->type->kind != TYPE_INT ||
          node->u.binary.rhs->type->kind != TYPE_INT) {
        ERROR_TYPE_KIND(node->u.binary.lhs);
      }
      node->type = TYPE_PRIM_INT;
    }
    break;
  case EXPR_ERR:
    ERROR_EXPR_KIND(node);
    break;
  }
}

static void type_check_stmt(TypeCtx *ctx, StmtNode *node) {
  switch (node->kind) {
  case STMT_RETURN:
    type_check_expr(ctx, node->u.ret.expr);
    break;
  case STMT_EXPR:
    type_check_expr(ctx, node->u.expr.expr);
    break;
  case STMT_BLOCK:
    for (StmtNode *n = node->u.block.stmt; n != NULL; n = n->next) {
      type_check_stmt(ctx, n);
    }
    break;
  case STMT_WHILE:
    type_check_expr(ctx, node->u.whil.cond);
    type_check_stmt(ctx, node->u.whil.then);
    break;
  case STMT_IF:
    type_check_expr(ctx, node->u.iff.cond);
    type_check_stmt(ctx, node->u.iff.then);
    if (node->u.iff.els != NULL) {
      type_check_stmt(ctx, node->u.iff.els);
    }
    break;
  case STMT_FOR:
    if (node->u.forr.init != NULL) {
      type_check_stmt(ctx, node->u.forr.init);
    }
    if (node->u.forr.cond != NULL) {
      type_check_expr(ctx, node->u.forr.cond);
    }
    if (node->u.forr.step != NULL) {
      type_check_expr(ctx, node->u.forr.step);
    }
    type_check_stmt(ctx, node->u.forr.then);
    break;
  case STMT_ERR:
    ERROR_STMT_KIND(node);
    break;
  }
}

void type_check(Arena *arena, FuncNode *prog) {
  TypeCtx ctx = type_ctx_create(arena);
  for (StmtNode *n = prog->body; n != NULL; n = n->next) {
    type_check_stmt(&ctx, n);
  }
  DEBUGF("Type checking done, found %zu new types", ctx.types.length);
  type_ctx_destroy(&ctx);
}

// -----------------------------------------------------------------------------
// Code generation
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

static uint32_t gen_uid(void) {
  static uint32_t uid = 1;
  return uid++;
}

static void gen_expr(ExprNode *node);

static void gen_addr(ExprNode *node) {
  switch (node->kind) {
  case EXPR_VAR:
    printf("  lea %d(%%rbp), %%rax\n", node->u.var.obj->offset);
    break;
  case EXPR_UN:
    switch (node->u.unary.op) {
    case UNOP_DEREF:
      gen_expr(node->u.unary.expr);
      break;
    default:
      ERROR_UNOP_KIND(node);
    }
    break;
  default:
    ERROR_EXPR_KIND(node);
  }
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
    case UNOP_ADDR:
      gen_addr(node->u.unary.expr);
      break;
    case UNOP_DEREF:
      gen_expr(node->u.unary.expr);
      printf("  mov (%%rax), %%rax\n");
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
  } break;
  case STMT_WHILE: {
    uint32_t c = gen_uid();
    printf(".L.begin.%d:\n", c);
    gen_expr(node->u.whil.cond);
    printf("  test %%rax, %%rax\n");
    printf("  je  .L.end.%d\n", c);
    gen_stmt(node->u.whil.then);
    printf("  jmp .L.begin.%d\n", c);
    printf(".L.end.%d:\n", c);
  } break;
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
  } break;
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
  Arena arena = arena_create(4096);
  FuncNode prog = parse(&arena, &lexer);
  if (prog.body == NULL) {
    error("failed to parse program\n");
    return EXIT_FAILURE;
  }

  if (lexer_peek(&lexer)->kind != TK_EOF) {
    error_at(start, lexer.token.lex.ptr,
             "unexpected token during parsing: '%.*s'\n", lexer.token.lex.len,
             lexer.token.lex.ptr);
    return EXIT_FAILURE;
  }

  resolve(&prog);
  type_check(&arena, &prog);
  DEBUG("AST:");
  ast_print(&prog);

  codegen(&prog);

  DEBUGF("total blocks allocated: %u", arena_total_blocks_allocated(&arena));
  DEBUGF("current bytes committed: %zu", arena.commited_size);
  DEBUGF("total bytes allocated: %zu", arena_total_bytes_allocated(&arena));

  arena_destroy(&arena);

  return EXIT_SUCCESS;
}
