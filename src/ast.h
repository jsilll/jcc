#ifndef JCC_AST_H
#define JCC_AST_H

#include "base.h"

#define ENUMERATE_TYPES(M)                                                     \
  M(TYPE_VOID)                                                                 \
  M(TYPE_INT)                                                                  \
  M(TYPE_PTR)

typedef enum { ENUMERATE_TYPES(GENERATE_ENUM) } TypeKind;

extern const char *const TYPE_KIND_STR[];

#define ENUMERATE_UNOPS(M)                                                     \
  M(UNOP_NEG)                                                                  \
  M(UNOP_NOT)                                                                  \
  M(UNOP_ADDR)                                                                 \
  M(UNOP_DEREF)

typedef enum { ENUMERATE_UNOPS(GENERATE_ENUM) } UnOpKind;

extern const char *const UNOP_KIND_STR[];

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

typedef enum { ENUMERATE_BINOPS(GENERATE_ENUM) } BinOpKind;

extern const char *const BINOP_KIND_STR[];

#define ENUMERATE_EXPRS(M)                                                     \
  M(EXPR_ERR)                                                                  \
  M(EXPR_NUM)                                                                  \
  M(EXPR_VAR)                                                                  \
  M(EXPR_UN)                                                                   \
  M(EXPR_BIN)

typedef enum { ENUMERATE_EXPRS(GENERATE_ENUM) } ExprKind;

extern const char *const EXPR_KIND_STR[];

#define ENUMERATE_STMTS(M)                                                     \
  M(STMT_ERR)                                                                  \
  M(STMT_EXPR)                                                                 \
  M(STMT_RETURN)                                                               \
  M(STMT_BLOCK)                                                                \
  M(STMT_IF)                                                                   \
  M(STMT_FOR)                                                                  \
  M(STMT_WHILE)

typedef enum { ENUMERATE_STMTS(GENERATE_ENUM) } StmtKind;

extern const char *const STMT_KIND_STR[];

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
      StmtNode *local;
    } var;
    struct {
      UnOpKind op;
      ExprNode *expr;
    } unary;
    struct {
      BinOpKind op;
      ExprNode *lhs;
      ExprNode *rhs;
    } binary;
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
      StmtNode *stmt;
    } block;
    struct {
      ExprNode *cond;
      StmtNode *then;
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
      StmtNode *then;
    } forr;
  } u;
};

typedef struct {
  StmtNode *body;
} FuncNode;

#endif // JCC_AST_H
