#include "desugar.h"

#include "support/base.h"

#include <stdlib.h>

typedef struct DesugarCtx {
  Arena *arena;
} DesugarCtx;

static void desugar_expr(DesugarCtx *ctx, ExprNode *expr) {
  switch (expr->kind) {
  case EXPR_NUM:
  case EXPR_VAR:
    break;
  case EXPR_UN:
    desugar_expr(ctx, expr->u.un.expr);
    break;
  case EXPR_BIN:
    desugar_expr(ctx, expr->u.bin.lhs);
    desugar_expr(ctx, expr->u.bin.rhs);
    switch (expr->u.bin.op) {
    case BINOP_ADD:
      if (expr->u.bin.lhs->type->kind == TY_PTR &&
          expr->u.bin.rhs->type->kind == TY_INT) {
        expr->u.bin.rhs = expr_init_binary(
            ctx->arena, expr->lex, BINOP_MUL, expr->u.bin.rhs,
            expr_init_int_from_value(ctx->arena, expr->lex,
                                     expr->u.bin.lhs->type->u.ptr.base->size));
      }
      break;
    case BINOP_SUB:
      if (expr->u.bin.lhs->type->kind == TY_PTR) {
        switch (expr->u.bin.rhs->type->kind) {
        case TY_INT:
          expr->u.bin.rhs = expr_init_binary(
              ctx->arena, expr->lex, BINOP_MUL, expr->u.bin.rhs,
              expr_init_int_from_value(
                  ctx->arena, expr->lex,
                  expr->u.bin.lhs->type->u.ptr.base->size));
          break;
        case TY_PTR:
          expr->u.bin.op = BINOP_DIV;
          expr->u.bin.lhs = expr_init_binary(ctx->arena, expr->lex, BINOP_SUB,
                                             expr->u.bin.lhs, expr->u.bin.rhs);
          expr->u.bin.rhs = expr_init_int_from_value(
              ctx->arena, expr->lex,
              expr->u.bin.lhs->u.bin.lhs->type->u.ptr.base->size);
          break;
        case TY_FUN:
          TODO("Handle TY_FUN");
          break;
        }
      }
    default:
      break;
    }
    break;
  case EXPR_IDX:
    desugar_expr(ctx, expr->u.index.array);
    desugar_expr(ctx, expr->u.index.index);
    break;
  case EXPR_CALL:
    desugar_expr(ctx, expr->u.call.func);
    break;
  }
}

static void desugar_stmt(DesugarCtx *ctx, StmtNode *stmt) {
  switch (stmt->kind) {
  case STMT_EXPR:
    desugar_expr(ctx, stmt->u.expr.expr);
    break;
  case STMT_RETURN:
    desugar_expr(ctx, stmt->u.ret.expr);
    break;
  case STMT_DECL:
    if (stmt->u.decl.expr != NULL) {
      desugar_expr(ctx, stmt->u.decl.expr);
    }
    break;
  case STMT_BLOCK:
    for (StmtNode *s = stmt->u.block.body; s != NULL; s = s->next) {
      desugar_stmt(ctx, s);
    }
    break;
  case STMT_WHILE:
    desugar_expr(ctx, stmt->u.whil.cond);
    desugar_stmt(ctx, stmt->u.whil.body);
    break;
  case STMT_IF:
    desugar_expr(ctx, stmt->u.iff.cond);
    desugar_stmt(ctx, stmt->u.iff.then);
    if (stmt->u.iff.elss != NULL) {
      desugar_stmt(ctx, stmt->u.iff.elss);
    }
    break;
  case STMT_FOR:
    desugar_stmt(ctx, stmt->u.forr.init);
    if (stmt->u.forr.cond != NULL) {
      desugar_expr(ctx, stmt->u.forr.cond);
    }
    if (stmt->u.forr.step != NULL) {
      desugar_expr(ctx, stmt->u.forr.step);
    }
    desugar_stmt(ctx, stmt->u.forr.body);
    break;
  }
}

void desugar(Arena *arena, FuncNode *function) {
  DesugarCtx ctx = {arena};
  for (FuncNode *func = function; func != NULL; func = func->next) {
    desugar_stmt(&ctx, func->body);
  }
}
