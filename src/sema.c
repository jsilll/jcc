#include "sema.h"

#include "adt/hash_set.h"

#include "support/base.h"

#include <stdlib.h>

DEFINE_VECTOR(SemaError, SemaErrorStream, sema_error_stream)

void sema_result_free(SemaResult *result) {
  sema_error_stream_free(&result->errors);
}

static uint64_t type_hash(const void *key) {
  const Type *type = key;
  switch (type->kind) {
  case TY_INT:
    return (uint64_t)key;
  case TY_PTR:
    return (uint64_t)type->u.ptr.base ^ ((uint64_t)type->kind << 32);
  case TY_FUN:
    TODO("Handle TY_FUN");
    break;
  }
  return 0;
}

static bool type_equals(const void *key1, const void *key2) {
  const Type *type1 = key1;
  const Type *type2 = key2;
  if (type1 == type2) {
    return true;
  }
  if (type1->kind != type2->kind) {
    return false;
  }
  switch (type1->kind) {
  case TY_INT:
    return true;
  case TY_PTR:
    return type1->u.ptr.base == type2->u.ptr.base;
  case TY_FUN:
    TODO("Handle TY_FUN");
    break;
  }
  return false;
}

typedef struct TypeCtx {
  Arena *arena;
  HashSet types;
  SemaResult *result;
} TypeCtx;

static void type_ctx_init(TypeCtx *ctx, Arena *arena, SemaResult *result) {
  hash_set_init(&ctx->types, 32, type_hash, type_equals);
  ctx->arena = arena;
  ctx->result = result;
}

static void type_ctx_free(TypeCtx *ctx) {
  DEBUGF("free types of capacity: %zu", ctx->types.capacity);
  hash_set_free(&ctx->types);
  ctx->arena = NULL;
  ctx->result = NULL;
}

static Type *type_ctx_register(TypeCtx *ctx, Type *type) {
  switch (type->kind) {
  case TY_INT:
    return type;
  case TY_PTR:
    type->u.ptr.base = type_ctx_register(ctx, type->u.ptr.base);
    return (Type *)hash_set_insert(&ctx->types, type);
  case TY_FUN:
    TODO("Handle TY_FUN");
    break;
  }
  return NULL;
}

static Type *type_ctx_make_ptr(TypeCtx *ctx, Type *base) {
  Type *type = arena_alloc(ctx->arena, sizeof(Type));
  type->kind = TY_PTR;
  type->u.ptr.base = base;
  Type *existing = type_ctx_register(ctx, type);
  if (existing != NULL) {
    arena_undo(ctx->arena, sizeof(Type));
    return existing;
  }
  return type;
}

static void type_check_assert(TypeCtx *ctx, StringView span, bool pred) {
  if (!pred) {
    sema_error_stream_push(&ctx->result->errors,
                           (SemaError){SEMA_ERR_MISMATCHED_TYPES, span});
  }
}

static void type_check_expr(TypeCtx *ctx, ExprNode *expr) {
  switch (expr->kind) {
  case EXPR_NUM:
    expr->type = TYPE_INT;
    break;
  case EXPR_VAR:
    expr->type = expr->u.var.decl->u.decl.type;
    break;
  case EXPR_UN:
    type_check_expr(ctx, expr->u.un.expr);
    switch (expr->u.un.op) {
    case UNOP_ADD:
      type_check_assert(ctx, expr->lex, expr->u.un.expr->type == TYPE_INT);
      expr->type = TYPE_INT;
      break;
    case UNOP_NEG:
      type_check_assert(ctx, expr->lex, expr->u.un.expr->type == TYPE_INT);
      expr->type = TYPE_INT;
      break;
    case UNOP_NOT:
      type_check_assert(ctx, expr->lex, expr->u.un.expr->type == TYPE_INT);
      expr->type = TYPE_INT;
      break;
    case UNOP_ADDR:
      expr->type = type_ctx_make_ptr(ctx, expr->u.un.expr->type);
      break;
    case UNOP_DEREF:
      type_check_assert(ctx, expr->lex, expr->u.un.expr->type->kind == TY_PTR);
      expr->type = expr->u.un.expr->type->u.ptr.base;
      break;
    }
    break;
  case EXPR_BIN:
    type_check_expr(ctx, expr->u.bin.lhs);
    type_check_expr(ctx, expr->u.bin.rhs);
    switch (expr->u.bin.op) {
    case BINOP_ADD:
      if (expr->u.bin.lhs->type == expr->u.bin.rhs->type) {
        type_check_assert(ctx, expr->lex,
                          expr->u.bin.lhs->type->kind == TY_INT);
        expr->type = TYPE_INT;
      } else {
        switch (expr->u.bin.lhs->type->kind) {
        case TY_INT:
          type_check_assert(ctx, expr->lex,
                            expr->u.bin.rhs->type->kind == TY_PTR);
          expr->type = expr->u.bin.rhs->type;
          break;
        case TY_PTR:
          type_check_assert(ctx, expr->lex,
                            expr->u.bin.rhs->type->kind == TY_INT);
          expr->type = expr->u.bin.lhs->type;
          break;
        case TY_FUN:
          TODO("Handle TY_FUN");
          break;
        }
      }
      break;
    case BINOP_SUB:
      if (expr->u.bin.lhs->type == expr->u.bin.rhs->type) {
        switch (expr->u.bin.lhs->type->kind) {
        case TY_INT:
          expr->type = TYPE_INT;
          break;
        case TY_PTR:
          expr->type = TYPE_INT;
          break;
        case TY_FUN:
          TODO("Handle TY_FUN");
          break;
        }
      } else {
        switch (expr->u.bin.lhs->type->kind) {
        case TY_INT:
          sema_error_stream_push(
              &ctx->result->errors,
              (SemaError){SEMA_ERR_MISMATCHED_TYPES, expr->lex});
          break;
        case TY_PTR:
          type_check_assert(ctx, expr->lex,
                            expr->u.bin.rhs->type->kind == TY_INT);
          expr->type = expr->u.bin.lhs->type;
          break;
        case TY_FUN:
          TODO("Handle TY_FUN");
          break;
        }
      }
      break;
    case BINOP_MUL:
      type_check_assert(ctx, expr->lex, expr->u.bin.lhs->type == TYPE_INT);
      type_check_assert(ctx, expr->lex, expr->u.bin.rhs->type == TYPE_INT);
      expr->type = TYPE_INT;
      break;
    case BINOP_DIV:
      type_check_assert(ctx, expr->lex, expr->u.bin.lhs->type == TYPE_INT);
      type_check_assert(ctx, expr->lex, expr->u.bin.rhs->type == TYPE_INT);
      expr->type = TYPE_INT;
      break;
    case BINOP_EQ:
    case BINOP_NE:
    case BINOP_LT:
    case BINOP_LE:
    case BINOP_GT:
    case BINOP_GE:
      type_check_assert(ctx, expr->lex, expr->u.bin.lhs->type == TYPE_INT);
      type_check_assert(ctx, expr->lex, expr->u.bin.rhs->type == TYPE_INT);
      expr->type = TYPE_INT;
      break;
    case BINOP_ASGN:
      type_check_assert(ctx, expr->lex,
                        expr->u.bin.lhs->type == expr->u.bin.rhs->type);
      expr->type = expr->u.bin.lhs->type;
      break;
    }
    break;
  case EXPR_IDX:
    type_check_expr(ctx, expr->u.index.array);
    type_check_expr(ctx, expr->u.index.index);
    break;
  case EXPR_CALL:
    // TODO: for now, we don't check the types of the arguments
    // type_check_expr(ctx, expr->u.call.func);
    break;
  }
}

static void type_check_stmt(TypeCtx *ctx, StmtNode *stmt) {
  switch (stmt->kind) {
  case STMT_EXPR:
    type_check_expr(ctx, stmt->u.expr.expr);
    break;
  case STMT_RETURN:
    type_check_expr(ctx, stmt->u.ret.expr);
    break;
  case STMT_DECL:
    stmt->u.decl.type = type_ctx_register(ctx, stmt->u.decl.type);
    if (stmt->u.decl.expr != NULL) {
      type_check_expr(ctx, stmt->u.decl.expr);
      if (stmt->u.decl.type != stmt->u.decl.expr->type) {
        sema_error_stream_push(
            &ctx->result->errors,
            (SemaError){SEMA_ERR_MISMATCHED_TYPES, stmt->lex});
      }
    }
    break;
  case STMT_BLOCK:
    for (StmtNode *n = stmt->u.block.body; n != NULL; n = n->next) {
      type_check_stmt(ctx, n);
    }
    break;
  case STMT_WHILE:
    type_check_expr(ctx, stmt->u.whil.cond);
    type_check_stmt(ctx, stmt->u.whil.body);
    break;
  case STMT_IF:
    type_check_expr(ctx, stmt->u.iff.cond);
    type_check_stmt(ctx, stmt->u.iff.then);
    if (stmt->u.iff.elss != NULL) {
      type_check_stmt(ctx, stmt->u.iff.elss);
    }
    break;
  case STMT_FOR:
    if (stmt->u.forr.init != NULL) {
      type_check_stmt(ctx, stmt->u.forr.init);
    }
    if (stmt->u.forr.cond != NULL) {
      type_check_expr(ctx, stmt->u.forr.cond);
    }
    if (stmt->u.forr.step != NULL) {
      type_check_expr(ctx, stmt->u.forr.step);
    }
    type_check_stmt(ctx, stmt->u.forr.body);
    break;
  }
}

SemaResult sema(Arena *arena, FuncNode *function) {
  SemaResult result = {0};
  sema_error_stream_init(&result.errors);

  TypeCtx ctx = {0};
  type_ctx_init(&ctx, arena, &result);

  for (FuncNode *func = function; func != NULL; func = func->next) {
    type_check_stmt(&ctx, func->body);
  }

  type_ctx_free(&ctx);

  return result;
}
