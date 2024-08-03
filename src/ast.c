#include "ast.h"

#define GENERATE_TYPE_VARIABLE(Name, Size)                                     \
  Type *TYPE_##Name = &(Type){TY_##Name, Size, {{0}}};

#define GENERATE_TYPE_STRING(Name, Size) #Name,

ENUMERATE_TYPES(GENERATE_TYPE_VARIABLE)

DEFINE_REPR_ENUM_MACRO(TypeKind, ENUMERATE_TYPES, GENERATE_TYPE_STRING)

DEFINE_REPR_ENUM(UnopKind, ENUMERATE_UNOPS)

DEFINE_REPR_ENUM(BinopKind, ENUMERATE_BINOPS)

DEFINE_REPR_ENUM(ExprKind, ENUMERATE_EXPRS)

DEFINE_REPR_ENUM(StmtKind, ENUMERATE_STMTS)

static void ast_expr_debug(FILE *out, ExprNode *expr, uint8_t indent) {
  for (uint8_t i = 0; i < indent; ++i) {
    fprintf(out, "  ");
  }
  switch (expr->kind) {
  case EXPR_VAR:
    if (expr->type != NULL) {
      fprintf(out, "%s<%s>: '%.*s' (%p)\n", ExprKind_Repr[expr->kind],
              TypeKind_Repr[expr->type->kind], (int)expr->lex.size,
              expr->lex.data, (void *)expr->u.var.decl);
    } else {
      fprintf(out, "%s: '%.*s' (%p)\n", ExprKind_Repr[expr->kind],
              (int)expr->lex.size, expr->lex.data, (void *)expr->u.var.decl);
    }
    break;
  case EXPR_NUM:
    if (expr->type != NULL) {
      fprintf(out, "%s<%s>: %ld\n", ExprKind_Repr[expr->kind],
              TypeKind_Repr[expr->type->kind], expr->u.num);
    } else {
      fprintf(out, "%s: %ld\n", ExprKind_Repr[expr->kind], expr->u.num);
    }
    break;
  case EXPR_UN:
    if (expr->type != NULL) {
      fprintf(out, "%s<%s>:\n", UnopKind_Repr[expr->u.un.op],
              TypeKind_Repr[expr->type->kind]);
    } else {
      fprintf(out, "%s:\n", UnopKind_Repr[expr->u.un.op]);
    }
    ast_expr_debug(out, expr->u.un.expr, indent + 1);
    break;
  case EXPR_BIN:
    if (expr->type != NULL) {
      fprintf(out, "%s<%s>:\n", BinopKind_Repr[expr->u.bin.op],
              TypeKind_Repr[expr->type->kind]);
    } else {
      fprintf(out, "%s:\n", BinopKind_Repr[expr->u.bin.op]);
    }
    ast_expr_debug(out, expr->u.bin.lhs, indent + 1);
    ast_expr_debug(out, expr->u.bin.rhs, indent + 1);
    break;
  case EXPR_IDX:
    if (expr->type != NULL) {
      fprintf(out, "INDEX<%s>:\n", TypeKind_Repr[expr->type->kind]);
    } else {
      fprintf(out, "INDEX:\n");
    }
    ast_expr_debug(out, expr->u.index.array, indent + 1);
    ast_expr_debug(out, expr->u.index.index, indent + 1);
    break;
  case EXPR_CALL:
    if (expr->type != NULL) {
      fprintf(out, "CALL<%s>:\n", TypeKind_Repr[expr->type->kind]);
    } else {
      fprintf(out, "CALL:\n");
    }
    ast_expr_debug(out, expr->u.call.func, indent + 1);
    for (ActualArg *arg = expr->u.call.args; arg != NULL; arg = arg->next) {
      ast_expr_debug(out, arg->expr, indent + 1);
    }
    break;
  }
}

static void ast_stmt_debug(FILE *out, StmtNode *stmt, uint8_t indent) {
  for (uint8_t i = 0; i < indent; ++i) {
    fprintf(out, "  ");
  }
  switch (stmt->kind) {
  case STMT_EXPR:
    fprintf(out, "%s:\n", StmtKind_Repr[stmt->kind]);
    ast_expr_debug(out, stmt->u.expr.expr, indent + 1);
    break;
  case STMT_RETURN:
    fprintf(out, "%s:\n", StmtKind_Repr[stmt->kind]);
    if (stmt->u.ret.expr != NULL) {
      ast_expr_debug(out, stmt->u.ret.expr, indent + 1);
    }
    break;
  case STMT_DECL:
    if (stmt->u.decl.type != NULL) {
      fprintf(out, "%s<%s>: '%.*s' (%p)\n", StmtKind_Repr[stmt->kind],
              TypeKind_Repr[stmt->u.decl.type->kind],
              (int)stmt->u.decl.name.size, stmt->u.decl.name.data,
              (void *)stmt);
    } else {
      fprintf(out, "%s: '%.*s' (%p)\n", StmtKind_Repr[stmt->kind],
              (int)stmt->u.decl.name.size, stmt->u.decl.name.data,
              (void *)stmt);
    }
    if (stmt->u.decl.expr != NULL) {
      ast_expr_debug(out, stmt->u.decl.expr, indent + 1);
    }
    break;
  case STMT_BLOCK:
    fprintf(out, "%s:\n", StmtKind_Repr[stmt->kind]);
    for (StmtNode *s = stmt->u.block.body; s != NULL; s = s->next) {
      ast_stmt_debug(out, s, indent + 1);
    }
    break;
  case STMT_WHILE:
    fprintf(out, "%s:\n", StmtKind_Repr[stmt->kind]);
    ast_expr_debug(out, stmt->u.whil.cond, indent + 1);
    ast_stmt_debug(out, stmt->u.whil.body, indent + 1);
    break;
  case STMT_IF:
    fprintf(out, "%s:\n", StmtKind_Repr[stmt->kind]);
    ast_expr_debug(out, stmt->u.iff.cond, indent + 1);
    ast_stmt_debug(out, stmt->u.iff.then, indent + 1);
    if (stmt->u.iff.elss != NULL) {
      ast_stmt_debug(out, stmt->u.iff.elss, indent + 1);
    }
    break;
  case STMT_FOR:
    fprintf(out, "%s:\n", StmtKind_Repr[stmt->kind]);
    ast_stmt_debug(out, stmt->u.forr.init, indent + 1);
    if (stmt->u.forr.cond != NULL) {
      ast_expr_debug(out, stmt->u.forr.cond, indent + 1);
    }
    if (stmt->u.forr.step != NULL) {
      ast_expr_debug(out, stmt->u.forr.step, indent + 1);
    }
    ast_stmt_debug(out, stmt->u.forr.body, indent + 1);
    break;
  }
}

void ast_debug(FILE *out, FuncNode *func) {
  if (func == NULL) {
    return;
  }
  ast_stmt_debug(out, func->body, 1);
}

Type *type_init(Arena *arena, TypeKind kind, uint8_t size) {
  Type *type = arena_alloc(arena, sizeof(Type));
  type->kind = kind;
  type->size = size;
  return type;
}

Type *type_init_ptr(Arena *arena, Type *base) {
  Type *type = type_init(arena, TY_PTR, 8);
  type->u.ptr.base = base;
  return type;
}

ExprNode *expr_init(Arena *arena, StringView lex, ExprKind kind) {
  ExprNode *expr = arena_alloc(arena, sizeof(ExprNode));
  expr->type = NULL;
  expr->kind = kind;
  expr->lex = lex;
  return expr;
}

ExprNode *expr_init_int(Arena *arena, StringView lex) {
  ExprNode *expr = expr_init(arena, lex, EXPR_NUM);
  char *end = NULL;
  expr->u.num = strtol(lex.data, &end, 10);
  assert(lex.data + lex.size == end);
  return expr;
}

ExprNode *expr_init_int_from_value(Arena *arena, StringView lex, int32_t num) {
  ExprNode *expr = expr_init(arena, lex, EXPR_NUM);
  expr->u.num = num;
  return expr;
}

ExprNode *expr_init_var(Arena *arena, StringView lex) {
  ExprNode *expr = expr_init(arena, lex, EXPR_VAR);
  return expr;
}

ExprNode *expr_init_unary(Arena *arena, StringView lex, UnOpKind op,
                          ExprNode *sub) {
  ExprNode *expr = expr_init(arena, lex, EXPR_UN);
  expr->u.un.expr = sub;
  expr->u.un.op = op;
  return expr;
}

ExprNode *expr_init_binary(Arena *arena, StringView lex, BinOpKind op,
                           ExprNode *lhs, ExprNode *rhs) {
  ExprNode *expr = expr_init(arena, lex, EXPR_BIN);
  expr->u.bin.lhs = lhs;
  expr->u.bin.rhs = rhs;
  expr->u.bin.op = op;
  return expr;
}

ExprNode *expr_init_call(Arena *arena, StringView lex, ExprNode *func,
                         ActualArg *args) {
  ExprNode *expr = expr_init(arena, lex, EXPR_CALL);
  expr->u.call.func = func;
  expr->u.call.args = args;
  return expr;
}

ExprNode *expr_init_index(Arena *arena, StringView lex, ExprNode *array,
                          ExprNode *index) {
  ExprNode *expr = expr_init(arena, lex, EXPR_IDX);
  expr->u.index.array = array;
  expr->u.index.index = index;
  return expr;
}

StmtNode *stmt_init(Arena *arena, StringView lex, StmtKind kind) {
  StmtNode *stmt = arena_alloc(arena, sizeof(StmtNode));
  stmt->kind = kind;
  stmt->lex = lex;
  stmt->next = NULL;
  return stmt;
}

StmtNode *stmt_init_return(Arena *arena, StringView lex, ExprNode *expr) {
  StmtNode *stmt = stmt_init(arena, lex, STMT_RETURN);
  stmt->u.ret.expr = expr;
  return stmt;
}

StmtNode *stmt_init_expr(Arena *arena, StringView lex, ExprNode *expr) {
  StmtNode *stmt = stmt_init(arena, lex, STMT_EXPR);
  stmt->u.expr.expr = expr;
  return stmt;
}

StmtNode *stmt_init_decl(Arena *arena, StringView lex, Type *type,
                         StringView name, ExprNode *expr) {
  StmtNode *stmt = stmt_init(arena, lex, STMT_DECL);
  stmt->u.decl.type = type;
  stmt->u.decl.name = name;
  stmt->u.decl.expr = expr;
  return stmt;
}

StmtNode *stmt_init_block(Arena *arena, StringView lex, StmtNode *body) {
  StmtNode *stmt = stmt_init(arena, lex, STMT_BLOCK);
  stmt->u.block.body = body;
  return stmt;
}

StmtNode *stmt_init_while(Arena *arena, StringView lex, ExprNode *cond,
                          StmtNode *body) {
  StmtNode *stmt = stmt_init(arena, lex, STMT_WHILE);
  stmt->u.whil.cond = cond;
  stmt->u.whil.body = body;
  return stmt;
}

StmtNode *stmt_init_if(Arena *arena, StringView lex, ExprNode *cond,
                       StmtNode *then, StmtNode *elss) {
  StmtNode *stmt = stmt_init(arena, lex, STMT_IF);
  stmt->u.iff.cond = cond;
  stmt->u.iff.then = then;
  stmt->u.iff.elss = elss;
  return stmt;
}

StmtNode *stmt_init_for(Arena *arena, StringView lex, StmtNode *init,
                        ExprNode *cond, ExprNode *step, StmtNode *body) {
  StmtNode *stmt = stmt_init(arena, lex, STMT_FOR);
  stmt->u.forr.init = init;
  stmt->u.forr.cond = cond;
  stmt->u.forr.step = step;
  stmt->u.forr.body = body;
  return stmt;
}
