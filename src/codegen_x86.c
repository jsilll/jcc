#include "codegen_x86.h"

#include "adt/hash_map.h"

#include <assert.h>
#include <stdlib.h>

static const char *const ARG_REG[] = {"%rdi", "%rsi", "%rdx",
                                      "%rcx", "%r8",  "%r9"};

static uint64_t stmt_hash(const void *key) { return (uint64_t)key; }

static bool stmt_equals(const void *key1, const void *key2) {
  return key1 == key2;
}

typedef struct CodegenCtx {
  FILE *out;
  HashMap offset;
  uint32_t label;
  FuncNode *func;
  int64_t stack_size;
} CodegenCtx;

static void codegen_ctx_init(CodegenCtx *ctx, FILE *out) {
  ctx->out = out;
  ctx->label = 0;
  ctx->func = NULL;
  ctx->stack_size = 0;
  hm_init(&ctx->offset, stmt_hash, stmt_equals);
}

static void codegen_ctx_clear(CodegenCtx *ctx) {
  ctx->label = 0;
  ctx->func = NULL;
  ctx->stack_size = 0;
  hm_clear(&ctx->offset);
}

static void codegen_ctx_free(CodegenCtx *ctx) {
  DEBUGF("free offsets map of capacity: %zu", ctx->offset.capacity);
  hm_free(&ctx->offset);
  ctx->out = NULL;
  ctx->label = 0;
  ctx->func = NULL;
  ctx->stack_size = 0;
}

static void assign_offsets_stmt(CodegenCtx *ctx, StmtNode *stmt) {
  switch (stmt->kind) {
  case STMT_DECL:
    ctx->stack_size += 8;
    hm_set(&ctx->offset, stmt, (void *)-ctx->stack_size);
    break;
  case STMT_BLOCK:
    for (StmtNode *s = stmt->u.block.body; s; s = s->next) {
      assign_offsets_stmt(ctx, s);
    }
    break;
  case STMT_WHILE:
    assign_offsets_stmt(ctx, stmt->u.whil.body);
    break;
  case STMT_IF:
    assign_offsets_stmt(ctx, stmt->u.iff.then);
    if (stmt->u.iff.elss) {
      assign_offsets_stmt(ctx, stmt->u.iff.elss);
    }
    break;
  case STMT_FOR:
    assign_offsets_stmt(ctx, stmt->u.forr.init);
    assign_offsets_stmt(ctx, stmt->u.forr.body);
    break;
  case STMT_EXPR:
  case STMT_RETURN:
    break;
  }
}

static void assign_offsets_func(CodegenCtx *ctx, FuncNode *func) {
  ctx->func = func;
  for (FormalArg *arg = func->args; arg != NULL; arg = arg->next) {
    assign_offsets_stmt(ctx, arg->decl);
  }
  for (StmtNode *stmt = func->body; stmt != NULL; stmt = stmt->next) {
    assign_offsets_stmt(ctx, stmt);
  }
  ctx->stack_size = ALIGN_UP(ctx->stack_size, 16);
}

static void codegen_x86_expr(CodegenCtx *ctx, ExprNode *expr);

static void codegen_x86_expr_addr(CodegenCtx *ctx, ExprNode *node) {
  int64_t offset = 0;
  switch (node->kind) {
  case EXPR_VAR:
    offset = (int64_t)hm_get(&ctx->offset, node->u.var.decl);
    assert(offset != 0);
    fprintf(ctx->out, "  lea %ld(%%rbp), %%rax\n", offset);
    break;
  case EXPR_UN:
    switch (node->u.un.op) {
    case UNOP_DEREF:
      codegen_x86_expr(ctx, node->u.un.expr);
      break;
    default:
      PANIC("codegen_x86_expr_address: invalid expression kind");
    }
    break;
  default:
    PANIC("codegen_x86_expr_address: invalid expression kind");
  }
}

static void codegen_x86_expr(CodegenCtx *ctx, ExprNode *expr) {
  switch (expr->kind) {
  case EXPR_NUM:
    fprintf(ctx->out, "  mov $%ld, %%rax\n", expr->u.num);
    break;
  case EXPR_VAR:
    codegen_x86_expr_addr(ctx, expr);
    fprintf(ctx->out, "  mov (%%rax), %%rax\n");
    break;
  case EXPR_UN:
    switch (expr->u.un.op) {
    case UNOP_ADD:
      codegen_x86_expr(ctx, expr->u.un.expr);
      break;
    case UNOP_NEG:
      codegen_x86_expr(ctx, expr->u.un.expr);
      fprintf(ctx->out, "  neg %%rax\n");
      break;
    case UNOP_NOT:
      codegen_x86_expr(ctx, expr->u.un.expr);
      fprintf(ctx->out, "  not %%rax\n");
      break;
    case UNOP_ADDR:
      codegen_x86_expr_addr(ctx, expr->u.un.expr);
      break;
    case UNOP_DEREF:
      codegen_x86_expr(ctx, expr->u.un.expr);
      fprintf(ctx->out, "  mov (%%rax), %%rax\n");
      break;
    }
    break;
  case EXPR_BIN:
    if (expr->u.bin.op == BINOP_ASGN) {
      codegen_x86_expr_addr(ctx, expr->u.bin.lhs);
      fprintf(ctx->out, "  push %%rax\n");
      codegen_x86_expr(ctx, expr->u.bin.rhs);
      fprintf(ctx->out, "  pop %%rdi\n");
      fprintf(ctx->out, "  mov %%rax, (%%rdi)\n");
      return;
    }
    codegen_x86_expr(ctx, expr->u.bin.rhs);
    fprintf(ctx->out, "  push %%rax\n");
    codegen_x86_expr(ctx, expr->u.bin.lhs);
    fprintf(ctx->out, "  pop %%rdi\n");
    switch (expr->u.bin.op) {
    case BINOP_ADD:
      fprintf(ctx->out, "  add %%rdi, %%rax\n");
      break;
    case BINOP_SUB:
      fprintf(ctx->out, "  sub %%rdi, %%rax\n");
      break;
    case BINOP_MUL:
      fprintf(ctx->out, "  imul %%rdi, %%rax\n");
      break;
    case BINOP_DIV:
      fprintf(ctx->out, "  cqo\n");
      fprintf(ctx->out, "  idiv %%rdi\n");
      break;
    case BINOP_EQ:
      fprintf(ctx->out, "  cmp %%rdi, %%rax\n");
      fprintf(ctx->out, "  sete %%al\n");
      fprintf(ctx->out, "  movzb %%al, %%rax\n");
      break;
    case BINOP_NE:
      fprintf(ctx->out, "  cmp %%rdi, %%rax\n");
      fprintf(ctx->out, "  setne %%al\n");
      fprintf(ctx->out, "  movzb %%al, %%rax\n");
      break;
    case BINOP_LT:
      fprintf(ctx->out, "  cmp %%rdi, %%rax\n");
      fprintf(ctx->out, "  setl %%al\n");
      fprintf(ctx->out, "  movzb %%al, %%rax\n");
      break;
    case BINOP_LE:
      fprintf(ctx->out, "  cmp %%rdi, %%rax\n");
      fprintf(ctx->out, "  setle %%al\n");
      fprintf(ctx->out, "  movzb %%al, %%rax\n");
      break;
    case BINOP_GT:
      fprintf(ctx->out, "  cmp %%rdi, %%rax\n");
      fprintf(ctx->out, "  setg %%al\n");
      fprintf(ctx->out, "  movzb %%al, %%rax\n");
      break;
    case BINOP_GE:
      fprintf(ctx->out, "  cmp %%rdi, %%rax\n");
      fprintf(ctx->out, "  setge %%al\n");
      fprintf(ctx->out, "  movzb %%al, %%rax\n");
      break;
    case BINOP_ASGN:
      PANIC("codegen_x86_expr: BINOP_ASGN not handled here");
      break;
    }
    break;
  case EXPR_IDX:
    TODO("codegen_x86_expr: handle EXPR_INDEX");
    break;
  case EXPR_CALL: {
    int8_t nargs = 0;
    for (ActualArg *arg = expr->u.call.args; arg != NULL;
         arg = arg->next, ++nargs) {
      codegen_x86_expr(ctx, arg->expr);
      fprintf(ctx->out, "  push %%rax\n");
    }
    assert(nargs <= 6);
    for (int arg = nargs - 1; arg >= 0; --arg) {
      fprintf(ctx->out, "  pop %s\n", ARG_REG[arg]);
    }
    fprintf(ctx->out, "  mov $0, %%rax\n");
    fprintf(ctx->out, "  call %.*s\n", (int)expr->u.call.func->lex.size,
            expr->u.call.func->lex.data);
  } break;
  }
}

static void codegen_x86_stmt(CodegenCtx *ctx, StmtNode *stmt) {
  int64_t offset = 0;
  switch (stmt->kind) {
  case STMT_EXPR:
    codegen_x86_expr(ctx, stmt->u.expr.expr);
    break;
  case STMT_RETURN:
    codegen_x86_expr(ctx, stmt->u.ret.expr);
    fprintf(ctx->out, "  jmp .L.return.%.*s\n", (int)ctx->func->lex.size,
            ctx->func->lex.data);
    break;
  case STMT_DECL:
    if (stmt->u.decl.expr != NULL) {
      offset = (int64_t)hm_get(&ctx->offset, stmt);
      assert(offset != 0);
      fprintf(ctx->out, "  lea %ld(%%rbp), %%rax\n", offset);
      fprintf(ctx->out, "  push %%rax\n");
      codegen_x86_expr(ctx, stmt->u.decl.expr);
      fprintf(ctx->out, "  pop %%rdi\n");
      fprintf(ctx->out, "  mov %%rax, (%%rdi)\n");
    }
    break;
  case STMT_BLOCK:
    for (StmtNode *s = stmt->u.block.body; s; s = s->next) {
      codegen_x86_stmt(ctx, s);
    }
    break;
  case STMT_WHILE:
    ++ctx->label;
    fprintf(ctx->out, ".L.begin.%d:\n", ctx->label);
    codegen_x86_expr(ctx, stmt->u.whil.cond);
    fprintf(ctx->out, "  test %%rax, %%rax\n");
    fprintf(ctx->out, "  je  .L.end.%d\n", ctx->label);
    codegen_x86_stmt(ctx, stmt->u.whil.body);
    fprintf(ctx->out, "  jmp .L.begin.%d\n", ctx->label);
    fprintf(ctx->out, ".L.end.%d:\n", ctx->label);
    break;
  case STMT_IF:
    ++ctx->label;
    codegen_x86_expr(ctx, stmt->u.iff.cond);
    fprintf(ctx->out, "  test %%rax, %%rax\n");
    fprintf(ctx->out, "  je .L.else.%u\n", ctx->label);
    codegen_x86_stmt(ctx, stmt->u.iff.then);
    fprintf(ctx->out, "  jmp .L.end.%u\n", ctx->label);
    fprintf(ctx->out, ".L.else.%u:\n", ctx->label);
    if (stmt->u.iff.elss != NULL) {
      codegen_x86_stmt(ctx, stmt->u.iff.elss);
    }
    fprintf(ctx->out, ".L.end.%u:\n", ctx->label);
    break;
  case STMT_FOR:
    ++ctx->label;
    codegen_x86_stmt(ctx, stmt->u.forr.init);
    fprintf(ctx->out, ".L.begin.%d:\n", ctx->label);
    if (stmt->u.forr.cond != NULL) {
      codegen_x86_expr(ctx, stmt->u.forr.cond);
      fprintf(ctx->out, "  test %%rax, %%rax\n");
      fprintf(ctx->out, "  je  .L.end.%d\n", ctx->label);
    }
    codegen_x86_stmt(ctx, stmt->u.forr.body);
    if (stmt->u.forr.step != NULL) {
      codegen_x86_expr(ctx, stmt->u.forr.step);
    }
    fprintf(ctx->out, "  jmp .L.begin.%d\n", ctx->label);
    fprintf(ctx->out, ".L.end.%d:\n", ctx->label);
    break;
  }
}

void codegen_x86(FILE *out, FuncNode *function) {
  CodegenCtx ctx = {0};
  codegen_ctx_init(&ctx, out);

  for (FuncNode *func = function; func != NULL; func = func->next) {
    assign_offsets_func(&ctx, func);
    DEBUGF("stack size: %ld", ctx.stack_size);

    fprintf(out, "  .globl %.*s\n", (int)func->lex.size, func->lex.data);
    fprintf(out, "%.*s:\n", (int)func->lex.size, func->lex.data);

    fprintf(out, "  push %%rbp\n");
    fprintf(out, "  mov %%rsp, %%rbp\n");
    fprintf(out, "  sub $%ld, %%rsp\n", ctx.stack_size);

    codegen_x86_stmt(&ctx, func->body);

    fprintf(out, ".L.return.%.*s:\n", (int)func->lex.size, func->lex.data);
    fprintf(out, "  mov %%rbp, %%rsp\n");
    fprintf(out, "  pop %%rbp\n");
    fprintf(out, "  ret\n");

    codegen_ctx_clear(&ctx);
  }

  codegen_ctx_free(&ctx);
}
