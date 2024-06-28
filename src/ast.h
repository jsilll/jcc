#ifndef JCC_AST_H
#define JCC_AST_H

#include "base.h"

#define ENUMERATE_TYPES(M)                                                     \
  M(TYPE_VOID)                                                                 \
  M(TYPE_INT)                                                                  \
  M(TYPE_PTR)
DECLARE_ENUM_WITH_REPR(TypeKind, ENUMERATE_TYPES)

#define ENUMERATE_UNOPS(M)                                                     \
  M(UNOP_NEG)                                                                  \
  M(UNOP_NOT)                                                                  \
  M(UNOP_ADDR)                                                                 \
  M(UNOP_DEREF)
DECLARE_ENUM_WITH_REPR(UnOpKind, ENUMERATE_UNOPS)

#define ENUMERATE_BINOPS(M)                                                    \
  M(BINOP_ADD)                                                                 \
  M(BINOP_SUB)                                                                 \
  M(BINOP_MUL)                                                                 \
  M(BINOP_DIV)                                                                 \
  M(BINOP_EQ)                                                                  \
  M(BINOP_NE)                                                                  \
  M(BINOP_LT)                                                                  \
  M(BINOP_LE)                                                                  \
  M(BINOP_GT)                                                                  \
  M(BINOP_GE)                                                                  \
  M(BINOP_ASGN)
DECLARE_ENUM_WITH_REPR(BinOpKind, ENUMERATE_BINOPS)

#define ENUMERATE_EXPRS(M)                                                     \
  M(EXPR_NUM)                                                                  \
  M(EXPR_VAR)                                                                  \
  M(EXPR_UN)                                                                   \
  M(EXPR_BIN)
DECLARE_ENUM_WITH_REPR(ExprKind, ENUMERATE_EXPRS)

#define ENUMERATE_STMTS(M)                                                     \
  M(STMT_EXPR)                                                                 \
  M(STMT_RETURN)                                                               \
  M(STMT_DECL)                                                                 \
  M(STMT_BLOCK)                                                                \
  M(STMT_WHILE)                                                                \
  M(STMT_IF)                                                                   \
  M(STMT_FOR)
DECLARE_ENUM_WITH_REPR(StmtKind, ENUMERATE_STMTS)

typedef struct Type Type;
typedef struct ExprNode ExprNode;
typedef struct StmtNode StmtNode;
typedef struct DeclNode DeclNode;

struct Type {
  TypeKind kind;
  union {
    struct {
      Type *base;
    } ptr;
  } u;
};

struct ExprNode {
  Type *type;
  ExprKind kind;
  StringView lex;
  union {
    int32_t num;
    struct {
      StmtNode *decl;
    } var;
    struct {
      UnOpKind op;
      ExprNode *expr;
    } un;
    struct {
      BinOpKind op;
      ExprNode *lhs;
      ExprNode *rhs;
    } bin;
  } u;
};

struct StmtNode {
  StmtKind kind;
  StringView lex;
  StmtNode *next;
  union {
    struct {
      ExprNode *expr;
    } ret;
    struct {
      ExprNode *expr;
    } expr;
    struct {
      StmtNode *body;
    } block;
    struct {
      StringView name;
      ExprNode *expr;
    } decl;
    struct {
      ExprNode *cond;
      StmtNode *body;
    } whil;
    struct {
      ExprNode *cond;
      StmtNode *then;
      StmtNode *elss;
    } iff;
    struct {
      StmtNode *init;
      ExprNode *cond;
      ExprNode *step;
      StmtNode *body;
    } forr;
  } u;
};

typedef struct {
  StmtNode *body;
} FuncNode;

void ast_debug(FILE *out, FuncNode *ast);

#endif // JCC_AST_H
