#ifndef CC_AST_H
#define CC_AST_H

#include "macros.h"
#include "string_view.h"

#define ENUMERATE_EXPRS(M)                                                     \
  M(EXPR_ERR)                                                                  \
  M(EXPR_NUM)                                                                  \
  M(EXPR_VAR)                                                                  \
  M(EXPR_NEG)                                                                  \
  M(EXPR_ADD)                                                                  \
  M(EXPR_SUB)                                                                  \
  M(EXPR_MUL)                                                                  \
  M(EXPR_DIV)                                                                  \
  M(EXPR_EQ)                                                                   \
  M(EXPR_NE)                                                                   \
  M(EXPR_LT)                                                                   \
  M(EXPR_LE)                                                                   \
  M(EXPR_GT)                                                                   \
  M(EXPR_GE)                                                                   \
  M(EXPR_ASSIGN)

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
typedef struct ExprNode {
  ExprKind kind;        // Expr kind
  StringView lex;       // Token lexeme
  struct ExprNode *lhs; // Left-hand side
  struct ExprNode *rhs; // Right-hand side
  union {
    float f;
    int32_t i;
    Object *obj;
  } value;
} ExprNode;

/// An AST statement node.
typedef struct StmtNode {
  StmtKind kind;         // Stmt kind
  StringView lex;        // Token lexeme
  struct StmtNode *init; // Initialization
  struct ExprNode *cond; // Condition
  struct ExprNode *step; // Step
  struct StmtNode *body; // Body
  struct StmtNode *els3; // Else
  struct StmtNode *next; // Next node
} StmtNode;

/// An AST function node.
typedef struct {
  StmtNode *body;      // Body
  Object *locals;      // Local variables
  uint32_t stack_size; // Stack size
} FuncNode;

#endif // CC_AST_H
