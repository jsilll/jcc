#include "ast.h"

DEFINE_ENUM_WITH_REPR(TypeKind, ENUMERATE_TYPES)

DEFINE_ENUM_WITH_REPR(UnopKind, ENUMERATE_UNOPS)

DEFINE_ENUM_WITH_REPR(BinopKind, ENUMERATE_BINOPS)

DEFINE_ENUM_WITH_REPR(ExprKind, ENUMERATE_EXPRS)

DEFINE_ENUM_WITH_REPR(StmtKind, ENUMERATE_STMTS)

static void ast_expr_debug(FILE *out, ExprNode *expr, uint8_t indent) {
  for (uint8_t i = 0; i < indent; ++i) {
    fprintf(out, "  ");
  }
  switch (expr->kind) {
  case EXPR_VAR:
    if (expr->type != NULL) {
      fprintf(out, "%s<%s>: '%.*s' (%p)\n", ExprKind_Repr[expr->kind],
              TypeKind_Repr[expr->type->kind], (int)expr->lex.size,
              expr->lex.data, (void *)expr->u.var.local);
    } else {
      fprintf(out, "%s: '%.*s' %p\n", ExprKind_Repr[expr->kind],
              (int)expr->lex.size, expr->lex.data, (void *)expr->u.var.local);
    }
    break;
  case EXPR_NUM:
    if (expr->type != NULL) {
      fprintf(out, "%s<%s>: %d\n", ExprKind_Repr[expr->kind],
              TypeKind_Repr[expr->type->kind], expr->u.num);
    } else {
      fprintf(out, "%s: %d\n", ExprKind_Repr[expr->kind], expr->u.num);
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
  }
}

static void ast_stmt_debug(FILE *out, StmtNode *stmt, uint8_t indent) {
  for (uint8_t i = 0; i < indent; ++i) {
    fprintf(out, "  ");
  }
  switch (stmt->kind) {
  case STMT_RETURN:
    fprintf(out, "%s:\n", StmtKind_Repr[stmt->kind]);
    if (stmt->u.ret.expr != NULL) {
      ast_expr_debug(out, stmt->u.ret.expr, indent + 1);
    }
    break;
  case STMT_EXPR:
    fprintf(out, "%s:\n", StmtKind_Repr[stmt->kind]);
    ast_expr_debug(out, stmt->u.expr.expr, indent + 1);
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
  for (StmtNode *stmt = func->body; stmt != NULL; stmt = stmt->next) {
    ast_stmt_debug(out, stmt, 1);
  }
}
