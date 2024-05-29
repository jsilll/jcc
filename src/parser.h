#ifndef CC_PARSER_H
#define CC_PARSER_H

#include "macros.h"
#include "string_view.h"

#define ENUMERATE_NODES(M)                                                     \
  M(ND_ERR)                                                                    \
  M(ND_EXPR_NUM)                                                               \
  M(ND_EXPR_VAR)                                                               \
  M(ND_EXPR_NEG)                                                               \
  M(ND_EXPR_ADD)                                                               \
  M(ND_EXPR_SUB)                                                               \
  M(ND_EXPR_MUL)                                                               \
  M(ND_EXPR_DIV)                                                               \
  M(ND_EXPR_EQ)                                                                \
  M(ND_EXPR_NE)                                                                \
  M(ND_EXPR_LT)                                                                \
  M(ND_EXPR_LE)                                                                \
  M(ND_EXPR_GT)                                                                \
  M(ND_EXPR_GE)                                                                \
  M(ND_EXPR_ASSIGN)                                                            \
  M(ND_STMT_EXPR)                                                              \
  M(ND_STMT_RETURN)                                                            \
  M(ND_STMT_BLOCK)                                                             \
  M(ND_STMT_IF)

/// The kind of a node.
typedef enum { ENUMERATE_NODES(GENERATE_ENUM) } NodeKind;

/// The string representation of a node kind.
static const char *const NODE_KIND_STR[] = {ENUMERATE_NODES(GENERATE_STRING)};

#define ERROR_NODE_KIND(node)                                                  \
  error("unexpected node kind during %s:\n-> kind: %s\n-> lex: '%.*s'\n",      \
        __FUNCTION__, NODE_KIND_STR[(node)->kind], (int)(node)->lex.len,       \
        (node)->lex.ptr)

/// An object.
typedef struct Object {
  StringView lex;      // Token lexeme
  int32_t offset;      // Offset from rbp
  struct Object *next; // Next object
} __attribute__((aligned(32))) Object;

/// An AST node.
typedef struct Node {
  NodeKind kind;     // Node kind
  StringView lex;    // Token lexeme
  struct Node *lhs;  // Left-hand side
  struct Node *rhs;  // Right-hand side
  struct Node *mhs;  // Middle-hand side
  struct Node *next; // Next node
  union {
    float f;
    int32_t i;
    Object *obj;
  } value;
} Node;

#endif // CC_PARSER_H
