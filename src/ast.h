#ifndef JCC_AST_H
#define JCC_AST_H

#include "arena.h"
#include "base.h"
#include <sys/types.h>

typedef struct Type Type;
typedef struct ExprNode ExprNode;
typedef struct StmtNode StmtNode;
typedef struct DeclNode DeclNode;

#define GENERATE_TYPE_ENUM(Name, Size) TY_##Name,

#define GENERATE_TYPE_EXTERN(Name, Size) extern Type *TYPE_##Name;

#define ENUMERATE_TYPES(M)                                                     \
  M(INT, 8)                                                                    \
  M(PTR, 8)

ENUMERATE_TYPES(GENERATE_TYPE_EXTERN)

DECLARE_REPR_ENUM_MACRO(TypeKind, ENUMERATE_TYPES, GENERATE_TYPE_ENUM)

#define ENUMERATE_UNOPS(M)                                                     \
  M(UNOP_ADD)                                                                  \
  M(UNOP_NEG)                                                                  \
  M(UNOP_NOT)                                                                  \
  M(UNOP_ADDR)                                                                 \
  M(UNOP_DEREF)

DECLARE_REPR_ENUM(UnOpKind, ENUMERATE_UNOPS)

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

DECLARE_REPR_ENUM(BinOpKind, ENUMERATE_BINOPS)

#define ENUMERATE_EXPRS(M)                                                     \
  M(EXPR_NUM)                                                                  \
  M(EXPR_VAR)                                                                  \
  M(EXPR_UN)                                                                   \
  M(EXPR_BIN)

DECLARE_REPR_ENUM(ExprKind, ENUMERATE_EXPRS)

#define ENUMERATE_STMTS(M)                                                     \
  M(STMT_EXPR)                                                                 \
  M(STMT_RETURN)                                                               \
  M(STMT_DECL)                                                                 \
  M(STMT_BLOCK)                                                                \
  M(STMT_WHILE)                                                                \
  M(STMT_IF)                                                                   \
  M(STMT_FOR)

DECLARE_REPR_ENUM(StmtKind, ENUMERATE_STMTS)

struct Type {
  TypeKind kind;
  uint8_t size;
  Type *base;
};

struct ExprNode {
  ExprKind kind;
  StringView lex;
  Type *type;
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
      Type *type;
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

Type *type_init(Arena *arena, TypeKind kind, uint8_t size);
Type *type_init_ptr(Arena *arena, Type *base);

ExprNode *expr_init(Arena *arena, StringView lex, ExprKind kind);
ExprNode *expr_init_int(Arena *arena, StringView lex);
ExprNode *expr_init_int_from_value(Arena *arena, StringView lex, int32_t num);
ExprNode *expr_init_var(Arena *arena, StringView lex);
ExprNode *expr_init_unary(Arena *arena, StringView lex, UnOpKind op,
                          ExprNode *sub);
ExprNode *expr_init_binary(Arena *arena, StringView lex, BinOpKind op,
                           ExprNode *lhs, ExprNode *rhs);

StmtNode *stmt_init(Arena *arena, StringView lex, StmtKind kind);
StmtNode *stmt_init_return(Arena *arena, StringView lex, ExprNode *expr);
StmtNode *stmt_init_expr(Arena *arena, StringView lex, ExprNode *expr);
StmtNode *stmt_init_decl(Arena *arena, StringView lex, Type *type,
                         StringView name, ExprNode *expr);
StmtNode *stmt_init_block(Arena *arena, StringView lex, StmtNode *body);
StmtNode *stmt_init_while(Arena *arena, StringView lex, ExprNode *cond,
                          StmtNode *body);
StmtNode *stmt_init_if(Arena *arena, StringView lex, ExprNode *cond,
                       StmtNode *then, StmtNode *elss);
StmtNode *stmt_init_for(Arena *arena, StringView lex, StmtNode *init,
                        ExprNode *cond, ExprNode *step, StmtNode *body);

#endif // JCC_AST_H
