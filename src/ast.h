#ifndef CC_AST_H
#define CC_AST_H

#include "macros.h"
#include "string_view.h"

#define ENUMERATE_UNOPS(M)                                                     \
  M(UNOP_NEG)                                                                  \
  M(UNOP_NOT)                                                                  \
  M(UNOP_ADDR)                                                                 \
  M(UNOP_DEREF)

/// The kind of a unary operator.
typedef enum { ENUMERATE_UNOPS(GENERATE_ENUM) } UnOpKind;

/// The string representation of a unary operator kind.
static const char *const UNOP_KIND_STR[] = {ENUMERATE_UNOPS(GENERATE_STRING)};

#define ERROR_BINOP_KIND(node)                                                 \
  error("unexpected binary operator kind during %s:\n-> kind: %s\n-> lex: "    \
        "'%.*s'\n",                                                            \
        __FUNCTION__, BINOP_KIND_STR[(node)->kind], (int)(node)->lex.len,      \
        (node)->lex.ptr)

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

/// The kind of a binary operator.
typedef enum { ENUMERATE_BINOPS(GENERATE_ENUM) } BinOpKind;

/// The string representation of a binary operator kind.
static const char *const BINOP_KIND_STR[] = {ENUMERATE_BINOPS(GENERATE_STRING)};

#define ERROR_UNOP_KIND(node)                                                  \
  error("unexpected unary operator kind during %s:\n-> kind: %s\n-> lex: "     \
        "'%.*s'\n",                                                            \
        __FUNCTION__, UNOP_KIND_STR[(node)->kind], (int)(node)->lex.len,       \
        (node)->lex.ptr)

#define ENUMERATE_EXPRS(M)                                                     \
  M(EXPR_ERR)                                                                  \
  M(EXPR_NUM)                                                                  \
  M(EXPR_VAR)                                                                  \
  M(EXPR_UN)                                                                   \
  M(EXPR_BIN)

/// The kind of a node.
typedef enum { ENUMERATE_EXPRS(GENERATE_ENUM) } ExprKind;

/// The string representation of a node kind.
static const char *const EXPR_KIND_STR[] = {ENUMERATE_EXPRS(GENERATE_STRING)};

/// An error message for an unexpected expression kind.
#define ERROR_EXPR_KIND(node)                                                  \
  error(                                                                       \
      "unexpected expression kind during %s:\n-> kind: %s\n-> lex: '%.*s'\n",  \
      __FUNCTION__, EXPR_KIND_STR[(node)->kind], (int)(node)->lex.len,         \
      (node)->lex.ptr)

#define ENUMERATE_STMTS(M)                                                     \
  M(STMT_ERR)                                                                  \
  M(STMT_EXPR)                                                                 \
  M(STMT_RETURN)                                                               \
  M(STMT_BLOCK)                                                                \
  M(STMT_IF)                                                                   \
  M(STMT_FOR)                                                                  \
  M(STMT_WHILE)

/// The kind of a statement.
typedef enum { ENUMERATE_STMTS(GENERATE_ENUM) } StmtKind;

/// The string representation of a statement kind.
static const char *const STMT_KIND_STR[] = {ENUMERATE_STMTS(GENERATE_STRING)};

/// An error message for an unexpected statement kind.
#define ERROR_STMT_KIND(node)                                                  \
  error("unexpected statement kind during %s:\n-> kind: %s\n-> lex: '%.*s'\n", \
        __FUNCTION__, STMT_KIND_STR[(node)->kind], (int)(node)->lex.len,       \
        (node)->lex.ptr)

/// An object.
typedef struct Object {
  StringView lex;      // Token lexeme
  int32_t offset;      // Offset from rbp
  struct Object *next; // Next object
} Object;

/// An AST expression node.
typedef struct ExprNode ExprNode;
typedef struct ExprNode {
  ExprKind kind;  // Expr kind
  StringView lex; // Token lexeme
  union {
    // clang-format off
    int32_t num;
    Object *var;
    struct { UnOpKind op; ExprNode *expr; } unary;
    struct { BinOpKind op; ExprNode *lhs, *rhs; } binary;
    // clang-format on
  } u;
} ExprNode;

/// An AST statement node.
typedef struct StmtNode StmtNode;
struct StmtNode {
  StmtKind kind;  // Stmt kind
  StringView lex; // Token lexeme
  StmtNode *next; // Next node
  union {
    // clang-format off
    struct { ExprNode *expr; } ret;
    struct { ExprNode *expr; } expr;
    struct { StmtNode *stmt; } block;
    struct { ExprNode *cond; StmtNode *then; } whil;
    struct { ExprNode *cond; StmtNode *then; StmtNode *els; } iff;
    struct { StmtNode *init; ExprNode *cond; ExprNode *step; StmtNode *then; } forr;
    // clang-format on
  } u;
};

/// An AST function node.
typedef struct {
  StmtNode *body;      // Body
  Object *locals;      // Local variables
  uint32_t stack_size; // Stack size
} FuncNode;

#endif // CC_AST_H
