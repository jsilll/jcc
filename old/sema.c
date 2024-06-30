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
