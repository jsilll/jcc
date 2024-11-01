#include "resolve.h"

#include "adt/hash_map.h"

#include <assert.h>
#include <stdlib.h>

void resolve_result_free(ResolveResult *result) {
  resolve_error_vec_free(&result->errors);
}

static uint64_t name_hash(const void *key) {
  return sv_hash((StringView *)key);
}

static bool name_equals(const void *key1, const void *key2) {
  return sv_equals((StringView *)key1, (StringView *)key2);
}

typedef struct Scopes {
  HashMap *scopes;
  size_t size;
  size_t init;
  size_t capacity;
} Scopes;

static void scopes_init(Scopes *scopes) {
  scopes->scopes = NULL;
  scopes->size = 0;
  scopes->init = 0;
  scopes->capacity = 0;
}

static void scopes_free(Scopes *scopes) {
  for (size_t i = 0; i < scopes->init; ++i) {
    DEBUGF("free scope of capacity: %zu", scopes->scopes[i].capacity);
    hm_free(scopes->scopes + i);
  }
  free(scopes->scopes);
  scopes->scopes = NULL;
  scopes->size = 0;
  scopes->init = 0;
  scopes->capacity = 0;
}

static void scopes_push(Scopes *scopes) {
  if (scopes->size == scopes->capacity) {
    scopes->capacity = GROW_CAP(scopes->capacity);
    void *tmp =
        realloc(scopes->scopes, scopes->capacity * sizeof(*scopes->scopes));
    assert(tmp != NULL);
    scopes->scopes = tmp;
  }
  if (scopes->size == scopes->init) {
    hm_init(scopes->scopes + scopes->size, name_hash, name_equals);
    ++scopes->init;
  }
  ++scopes->size;
}

static void scopes_pop(Scopes *scopes) {
  assert(scopes->size > 0);
  hm_clear(scopes->scopes + scopes->size - 1);
  --scopes->size;
}

static StmtNode *scopes_insert(Scopes *scopes, StringView *name,
                               StmtNode *decl) {
  assert(scopes->size > 0);
  return hm_set(scopes->scopes + scopes->size - 1, name, decl);
}

static StmtNode *scopes_lookup(Scopes *scopes, StringView *name) {
  for (size_t i = scopes->size; i > 0; --i) {
    StmtNode *decl = hm_get(scopes->scopes + i - 1, name);
    if (decl != NULL) {
      return decl;
    }
  }
  return NULL;
}

typedef struct ResolveCtx {
  Scopes scopes;
  ResolveResult *result;
} ResolveCtx;

static void resolve_ctx_init(ResolveCtx *ctx, ResolveResult *result) {
  scopes_init(&ctx->scopes);
  ctx->result = result;
}

static void resolve_ctx_free(ResolveCtx *ctx) { scopes_free(&ctx->scopes); }

static void resolve_expr(ResolveCtx *ctx, ExprNode *expr) {
  switch (expr->kind) {
  case EXPR_NUM:
    break;
  case EXPR_UN:
    resolve_expr(ctx, expr->u.un.expr);
    break;
  case EXPR_BIN:
    resolve_expr(ctx, expr->u.bin.lhs);
    resolve_expr(ctx, expr->u.bin.rhs);
    break;
  case EXPR_VAR: {
    StmtNode *decl = scopes_lookup(&ctx->scopes, &expr->lex);
    if (decl == NULL) {
      resolve_error_vec_push(
          &ctx->result->errors,
          (ResolveError){RESOLVE_ERR_UNDECLARED, expr->lex});
    }
    expr->u.var.decl = decl;
  } break;
  case EXPR_IDX:
    resolve_expr(ctx, expr->u.index.array);
    resolve_expr(ctx, expr->u.index.index);
    break;
  case EXPR_CALL:
    // TODO: for now we don't resolve function calls, for testing purposes
    // resolve_expr(ctx, expr->u.call.func);
    for (ActualArg *arg = expr->u.call.args; arg != NULL; arg = arg->next) {
      resolve_expr(ctx, arg->expr);
    }
    break;
  }
}

static void resolve_stmt(ResolveCtx *ctx, StmtNode *stmt) {
  switch (stmt->kind) {
  case STMT_EXPR:
    resolve_expr(ctx, stmt->u.expr.expr);
    break;
  case STMT_RETURN:
    resolve_expr(ctx, stmt->u.ret.expr);
    break;
  case STMT_DECL:
    if (stmt->u.decl.expr != NULL) {
      resolve_expr(ctx, stmt->u.decl.expr);
    }
    if (scopes_insert(&ctx->scopes, &stmt->u.decl.name, stmt) != NULL) {
      resolve_error_vec_push(
          &ctx->result->errors,
          (ResolveError){RESOLVE_ERR_DUPLICATE, stmt->u.decl.name});
    }
    break;
  case STMT_BLOCK:
    scopes_push(&ctx->scopes);
    for (StmtNode *s = stmt->u.block.body; s != NULL; s = s->next) {
      resolve_stmt(ctx, s);
    }
    scopes_pop(&ctx->scopes);
    break;
  case STMT_WHILE:
    resolve_expr(ctx, stmt->u.whil.cond);
    resolve_stmt(ctx, stmt->u.whil.body);
    break;
  case STMT_IF:
    resolve_expr(ctx, stmt->u.iff.cond);
    resolve_stmt(ctx, stmt->u.iff.then);
    if (stmt->u.iff.elss != NULL) {
      resolve_stmt(ctx, stmt->u.iff.elss);
    }
    break;
  case STMT_FOR:
    resolve_stmt(ctx, stmt->u.forr.init);
    if (stmt->u.forr.cond != NULL) {
      resolve_expr(ctx, stmt->u.forr.cond);
    }
    if (stmt->u.forr.step != NULL) {
      resolve_expr(ctx, stmt->u.forr.step);
    }
    resolve_stmt(ctx, stmt->u.forr.body);
    break;
  }
}

ResolveResult resolve(FuncNode *function) {
  ResolveResult result = {0};
  resolve_error_vec_init(&result.errors);

  ResolveCtx ctx = {0};
  resolve_ctx_init(&ctx, &result);

  for (FuncNode *func = function; func != NULL; func = func->next) {
    scopes_push(&ctx.scopes);
    for (FormalArg *arg = func->args; arg != NULL; arg = arg->next) {
      scopes_insert(&ctx.scopes, &arg->decl->u.decl.name, arg->decl);
    }
    for (StmtNode *stmt = func->body; stmt != NULL; stmt = stmt->next) {
      resolve_stmt(&ctx, stmt);
    }
    scopes_pop(&ctx.scopes);
  }

  resolve_ctx_free(&ctx);

  return result;
}
