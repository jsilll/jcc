// -----------------------------------------------------------------------------
// Prelude
// -----------------------------------------------------------------------------

#include "arena.h"
#include "ast.h"
#include "error.h"
#include "hash_map.h"
#include "hash_set.h"
#include "lexer.h"
#include "string_view.h"
#include "type_check.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// -----------------------------------------------------------------------------
// Type checking
// -----------------------------------------------------------------------------

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

// -----------------------------------------------------------------------------
// Code generation
// -----------------------------------------------------------------------------

static int align_to(int n, int align) {
  return (n + align - 1) / align * align;
}

static void assign_local_offsets(FuncNode *prog) {
  int offset = 0;
  for (Object *o = prog->locals; o != NULL; o = o->next) {
    offset += 8;
    o->offset = -offset;
  }
  prog->stack_size = align_to(offset, 16);
}

static uint32_t gen_uid(void) {
  static uint32_t uid = 1;
  return uid++;
}

static void gen_expr(ExprNode *node);

static void gen_addr(ExprNode *node) {
  switch (node->kind) {
  case EXPR_VAR:
    printf("  lea %d(%%rbp), %%rax\n", node->u.var.obj->offset);
    break;
  case EXPR_UN:
    switch (node->u.unary.op) {
    case UNOP_DEREF:
      gen_expr(node->u.unary.expr);
      break;
    default:
      ERROR_UNOP_KIND(node);
    }
    break;
  default:
    ERROR_EXPR_KIND(node);
  }
}

static void gen_expr(ExprNode *node) {
  switch (node->kind) {
  case EXPR_NUM:
    printf("  mov $%d, %%rax\n", node->u.num);
    break;
  case EXPR_VAR:
    gen_addr(node);
    printf("  mov (%%rax), %%rax\n");
    break;
  case EXPR_UN:
    switch (node->u.unary.op) {
    case UNOP_NEG:
      gen_expr(node->u.unary.expr);
      printf("  neg %%rax\n");
      break;
    case UNOP_ADDR:
      gen_addr(node->u.unary.expr);
      break;
    case UNOP_DEREF:
      gen_expr(node->u.unary.expr);
      printf("  mov (%%rax), %%rax\n");
      break;
    default:
      ERROR_UNOP_KIND(node);
    }
    break;
  case EXPR_BIN:
    if (node->u.binary.op == BINOP_ASGN) {
      gen_addr(node->u.binary.lhs);
      printf("  push %%rax\n");
      gen_expr(node->u.binary.rhs);
      printf("  pop %%rdi\n");
      printf("  mov %%rax, (%%rdi)\n");
      return;
    }
    gen_expr(node->u.binary.rhs);
    printf("  push %%rax\n");
    gen_expr(node->u.binary.lhs);
    printf("  pop %%rdi\n");
    switch (node->u.binary.op) {
    case BINOP_ADD:
      printf("  add %%rdi, %%rax\n");
      break;
    case BINOP_SUB:
      printf("  sub %%rdi, %%rax\n");
      break;
    case BINOP_MUL:
      printf("  imul %%rdi, %%rax\n");
      break;
    case BINOP_DIV:
      printf("  cqo\n");
      printf("  idiv %%rdi\n");
      break;
    case BINOP_EQ:
      printf("  cmp %%rdi, %%rax\n");
      printf("  sete %%al\n");
      printf("  movzb %%al, %%rax\n");
      break;
    case BINOP_NE:
      printf("  cmp %%rdi, %%rax\n");
      printf("  setne %%al\n");
      printf("  movzb %%al, %%rax\n");
      break;
    case BINOP_LT:
      printf("  cmp %%rdi, %%rax\n");
      printf("  setl %%al\n");
      printf("  movzb %%al, %%rax\n");
      break;
    case BINOP_LE:
      printf("  cmp %%rdi, %%rax\n");
      printf("  setle %%al\n");
      printf("  movzb %%al, %%rax\n");
      break;
    case BINOP_GT:
      printf("  cmp %%rdi, %%rax\n");
      printf("  setg %%al\n");
      printf("  movzb %%al, %%rax\n");
      break;
    case BINOP_GE:
      printf("  cmp %%rdi, %%rax\n");
      printf("  setge %%al\n");
      printf("  movzb %%al, %%rax\n");
      break;
    default:
      ERROR_BINOP_KIND(node);
    }
    break;
  case EXPR_ERR:
    ERROR_EXPR_KIND(node);
    break;
  }
}

static void gen_stmt(StmtNode *node) {
  switch (node->kind) {
  case STMT_EXPR:
    gen_expr(node->u.expr.expr);
    break;
  case STMT_RETURN:
    gen_expr(node->u.ret.expr);
    printf("  jmp .L.return\n");
    break;
  case STMT_BLOCK:
    for (StmtNode *n = node->u.block.stmt; n != NULL; n = n->next) {
      gen_stmt(n);
    }
    break;
  case STMT_IF: {
    uint32_t uid = gen_uid();
    gen_expr(node->u.iff.cond);
    printf("  test %%rax, %%rax\n");
    printf("  je .L.else.%u\n", uid);
    gen_stmt(node->u.iff.then);
    printf("  jmp .L.end.%u\n", uid);
    printf(".L.else.%u:\n", uid);
    if (node->u.iff.els != NULL) {
      gen_stmt(node->u.iff.els);
    }
    printf(".L.end.%u:\n", uid);
  } break;
  case STMT_WHILE: {
    uint32_t c = gen_uid();
    printf(".L.begin.%d:\n", c);
    gen_expr(node->u.whil.cond);
    printf("  test %%rax, %%rax\n");
    printf("  je  .L.end.%d\n", c);
    gen_stmt(node->u.whil.then);
    printf("  jmp .L.begin.%d\n", c);
    printf(".L.end.%d:\n", c);
  } break;
  case STMT_FOR: {
    uint32_t c = gen_uid();
    gen_stmt(node->u.forr.init);
    printf(".L.begin.%d:\n", c);
    if (node->u.forr.cond != NULL) {
      gen_expr(node->u.forr.cond);
      printf("  test %%rax, %%rax\n");
      printf("  je  .L.end.%d\n", c);
    }
    gen_stmt(node->u.forr.then);
    if (node->u.forr.step != NULL) {
      gen_expr(node->u.forr.step);
    }
    printf("  jmp .L.begin.%d\n", c);
    printf(".L.end.%d:\n", c);
  } break;
  case STMT_ERR:
    ERROR_STMT_KIND(node);
    break;
  }
}

static void codegen(FuncNode *prog) {
  assign_local_offsets(prog);
  DEBUGF("stack size: %d", prog->stack_size);
  printf("  .globl main\n");
  printf("main:\n");
  printf("  push %%rbp\n");
  printf("  mov %%rsp, %%rbp\n");
  printf("  sub $%d, %%rsp\n", prog->stack_size);
  for (StmtNode *n = prog->body; n != NULL; n = n->next) {
    gen_stmt(n);
  }
  printf(".L.return:\n");
  printf("  mov %%rbp, %%rsp\n");
  printf("  pop %%rbp\n");
  printf("  ret\n");
}

// -----------------------------------------------------------------------------
// Main
// -----------------------------------------------------------------------------

int main(int argc, char *argv[]) {
  if (argc != 2) {
    error("usage: %s <expr>\n", argv[0]);
    return EXIT_FAILURE;
  }

  char *start = argv[1];
  Lexer lexer = lexer_create(start);
  DEBUG("Tokens:");
  for (Token *t = lexer_peek(&lexer); t->kind != TK_EOF;
       t = lexer_next(&lexer)) {
    switch (lexer_peek(&lexer)->kind) {
    case TK_ERR:
    case TK_IDENT:
      fprintf(stderr, "%s: '%.*s'\n", TOKEN_KIND_STR[t->kind],
              (int)lexer_peek(&lexer)->lex.len, lexer_peek(&lexer)->lex.ptr);
      break;
    case TK_NUM:
      fprintf(stderr, "%s: %d\n", TOKEN_KIND_STR[t->kind],
              lexer_peek(&lexer)->u.num);
      break;
    default:
      fprintf(stderr, "%s\n", TOKEN_KIND_STR[t->kind]);
    }
  }

  lexer = lexer_create(start);
  Arena arena = arena_create(4096);
  FuncNode prog = parse(&arena, &lexer);
  if (prog.body == NULL) {
    error("failed to parse program\n");
    return EXIT_FAILURE;
  }

  if (lexer_peek(&lexer)->kind != TK_EOF) {
    error_at(start, lexer.token.lex.ptr,
             "unexpected token during parsing: '%.*s'\n", lexer.token.lex.len,
             lexer.token.lex.ptr);
    return EXIT_FAILURE;
  }

  resolve(&prog);
  type_check(&arena, &prog);
  DEBUG("AST:");
  ast_print(&prog);

  codegen(&prog);

  DEBUGF("total blocks allocated: %u", arena_total_blocks_allocated(&arena));
  DEBUGF("current bytes committed: %zu", arena.commited_size);
  DEBUGF("total bytes allocated: %zu", arena_total_bytes_allocated(&arena));

  arena_destroy(&arena);

  return EXIT_SUCCESS;
}
