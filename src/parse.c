#include "parse.h"
#include "base.h"

#include <assert.h>
#include <stdint.h>
#include <stdlib.h>

DEFINE_VECTOR(ParseError, ParseErrorStream, parse_error_stream)

void parse_result_free(ParseResult *result) {
  parse_error_stream_free(&result->errors);
}

static ExprNode *expr_init(Arena *arena, StringView lex, ExprKind kind) {
  ExprNode *expr = arena_alloc(arena, sizeof(ExprNode));
  expr->type = NULL;
  expr->kind = kind;
  expr->lex = lex;
  return expr;
}

static ExprNode *expr_init_integer(Arena *arena, Token *token) {
  ExprNode *expr = expr_init(arena, token->lex, EXPR_NUM);
  char *end = NULL;
  expr->u.num = strtol(token->lex.data, &end, 10);
  assert(token->lex.data + token->lex.size == end);
  return expr;
}

static ExprNode *expr_init_variable(Arena *arena, StringView lex) {
  ExprNode *expr = expr_init(arena, lex, EXPR_VAR);
  return expr;
}

static ExprNode *expr_init_unary(Arena *arena, StringView lex, UnOpKind op,
                                 ExprNode *sub) {
  ExprNode *expr = expr_init(arena, lex, EXPR_UN);
  expr->u.un.expr = sub;
  expr->u.un.op = op;
  return expr;
}

static ExprNode *expr_init_binary(Arena *arena, StringView lex, BinOpKind op,
                                  ExprNode *lhs, ExprNode *rhs) {
  ExprNode *expr = expr_init(arena, lex, EXPR_BIN);
  expr->u.bin.lhs = lhs;
  expr->u.bin.rhs = rhs;
  expr->u.bin.op = op;
  return expr;
}

static StmtNode *stmt_init(Arena *arena, StringView lex, StmtKind kind) {
  StmtNode *stmt = arena_alloc(arena, sizeof(StmtNode));
  stmt->kind = kind;
  stmt->lex = lex;
  stmt->next = NULL;
  return stmt;
}

static StmtNode *stmt_init_return(Arena *arena, StringView lex,
                                  ExprNode *expr) {
  StmtNode *stmt = stmt_init(arena, lex, STMT_RETURN);
  stmt->u.ret.expr = expr;
  return stmt;
}

static StmtNode *stmt_init_expr(Arena *arena, StringView lex, ExprNode *expr) {
  StmtNode *stmt = stmt_init(arena, lex, STMT_EXPR);
  stmt->u.expr.expr = expr;
  return stmt;
}

static StmtNode *stmt_init_decl(Arena *arena, StringView lex, StringView name,
                                ExprNode *expr) {
  StmtNode *stmt = stmt_init(arena, lex, STMT_DECL);
  stmt->u.decl.name = name;
  stmt->u.decl.expr = expr;
  return stmt;
}

static StmtNode *stmt_init_block(Arena *arena, StringView lex, StmtNode *body) {
  StmtNode *stmt = stmt_init(arena, lex, STMT_BLOCK);
  stmt->u.block.body = body;
  return stmt;
}

static StmtNode *stmt_init_while(Arena *arena, StringView lex, ExprNode *cond,
                                 StmtNode *body) {
  StmtNode *stmt = stmt_init(arena, lex, STMT_WHILE);
  stmt->u.whil.cond = cond;
  stmt->u.whil.body = body;
  return stmt;
}

static StmtNode *stmt_init_if(Arena *arena, StringView lex, ExprNode *cond,
                              StmtNode *then, StmtNode *elss) {
  StmtNode *stmt = stmt_init(arena, lex, STMT_IF);
  stmt->u.iff.cond = cond;
  stmt->u.iff.then = then;
  stmt->u.iff.elss = elss;
  return stmt;
}

static StmtNode *stmt_init_for(Arena *arena, StringView lex, StmtNode *init,
                               ExprNode *cond, ExprNode *step, StmtNode *body) {
  StmtNode *stmt = stmt_init(arena, lex, STMT_FOR);
  stmt->u.forr.init = init;
  stmt->u.forr.cond = cond;
  stmt->u.forr.step = step;
  stmt->u.forr.body = body;
  return stmt;
}

typedef struct ParseCtx {
  uint32_t idx;
  Arena *arena;
  ParseResult *result;
  TokenStream *tokens;
} ParseCtx;

static Token *peek_token(ParseCtx *ctx) {
  if (ctx->idx == ctx->tokens->size) {
    return NULL;
  }
  return ctx->tokens->data + ctx->idx;
}

static Token *next_token(ParseCtx *ctx) {
  if (ctx->idx == ctx->tokens->size) {
    return NULL;
  }
  return ctx->tokens->data + (ctx->idx++);
}

static Token *peek_some(ParseCtx *ctx) {
  Token *token = peek_token(ctx);
  if (token == NULL) {
    parse_error_stream_push(
        &ctx->result->errors,
        (ParseError){PARSE_ERR_EXPECTED_SOME, NULL, TK_IDENT});
    return NULL;
  }
  return token;
}

static Token *expect_token(ParseCtx *ctx, TokenKind kind) {
  Token *token = peek_token(ctx);
  if (token == NULL) {
    parse_error_stream_push(&ctx->result->errors,
                            (ParseError){PARSE_ERR_UNEXPECTED_EOF, NULL, kind});
    return NULL;
  } else if (token->kind != kind) {
    parse_error_stream_push(
        &ctx->result->errors,
        (ParseError){PARSE_ERR_EXPECTED_TOKEN, token, kind});
    return NULL;
  }
  ++ctx->idx;
  return token;
}

typedef struct BindingPower {
  uint8_t lhs;
  uint8_t rhs;
} BindingPower;

typedef struct BinOpPair {
  BinOpKind op;
  BindingPower bind;
} BinOpPair;

static BinOpPair try_into_binop(TokenKind kind) {
  switch (kind) {
  case TK_EQ:
    return (BinOpPair){BINOP_ASGN, (BindingPower){2, 1}};
  case TK_EQ_EQ:
    return (BinOpPair){BINOP_EQ, (BindingPower){3, 4}};
  case TK_BANG_EQ:
    return (BinOpPair){BINOP_NE, (BindingPower){3, 4}};
  case TK_LT:
    return (BinOpPair){BINOP_LT, (BindingPower){5, 6}};
  case TK_LT_EQ:
    return (BinOpPair){BINOP_LE, (BindingPower){5, 6}};
  case TK_GT:
    return (BinOpPair){BINOP_GT, (BindingPower){5, 6}};
  case TK_GT_EQ:
    return (BinOpPair){BINOP_GE, (BindingPower){5, 6}};
  case TK_PLUS:
    return (BinOpPair){BINOP_ADD, (BindingPower){7, 8}};
  case TK_MINUS:
    return (BinOpPair){BINOP_SUB, (BindingPower){7, 8}};
  case TK_STAR:
    return (BinOpPair){BINOP_MUL, (BindingPower){9, 10}};
  case TK_SLASH:
    return (BinOpPair){BINOP_DIV, (BindingPower){9, 10}};
  default:
    return (BinOpPair){BINOP_ADD, (BindingPower){0, 0}};
  }
}

static ExprNode *parse_expr(ParseCtx *ctx, uint8_t prec);

static ExprNode *parse_expr_primary(ParseCtx *ctx) {
  Token *token = next_token(ctx);
  switch (token->kind) {
  case TK_NUM:
    return expr_init_integer(ctx->arena, token);
  case TK_IDENT:
    return expr_init_variable(ctx->arena, token->lex);
  case TK_PLUS:
    return parse_expr_primary(ctx);
  case TK_MINUS:
    return expr_init_unary(ctx->arena, token->lex, UNOP_NEG,
                           parse_expr(ctx, 99));
  case TK_AMP:
    return expr_init_unary(ctx->arena, token->lex, UNOP_ADDR,
                           parse_expr(ctx, 99));
  case TK_STAR:
    return expr_init_unary(ctx->arena, token->lex, UNOP_DEREF,
                           parse_expr(ctx, 99));
  case TK_LPAREN: {
    ExprNode *expr = parse_expr(ctx, 0);
    expect_token(ctx, TK_RPAREN);
    return expr;
  }
  default:
    parse_error_stream_push(
        &ctx->result->errors,
        (ParseError){PARSE_ERR_UNEXPECTED_TOKEN, NULL, TK_IDENT});
    return NULL;
  }
}

static ExprNode *parse_expr(ParseCtx *ctx, uint8_t prec) {
  ExprNode *lhs = parse_expr_primary(ctx);
  while (true) {
    Token *token = peek_token(ctx);
    if (token == NULL) {
      return lhs;
    }
    BinOpPair bp = try_into_binop(token->kind);
    if (bp.bind.lhs <= prec) {
      return lhs;
    }
    next_token(ctx);
    lhs = expr_init_binary(ctx->arena, token->lex, bp.op, lhs,
                           parse_expr(ctx, bp.bind.rhs));
  }
  return lhs;
}

static StmtNode *parse_stmt(ParseCtx *ctx);

static StmtNode *parse_stmt_block(ParseCtx *ctx) {
  Token *begin = next_token(ctx);
  StmtNode head = {0};
  StmtNode *curr = &head;
  Token *token = peek_token(ctx);
  while (token != NULL && token->kind != TK_RBRACE) {
    curr->next = parse_stmt(ctx);
    token = peek_token(ctx);
    curr = curr->next;
  }
  expect_token(ctx, TK_RBRACE);
  return stmt_init_block(ctx->arena, begin->lex, head.next);
}

static StmtNode *parse_stmt(ParseCtx *ctx) {
  Token *token = peek_some(ctx);
  switch (token->kind) {
  case TK_LBRACE:
    return parse_stmt_block(ctx);
  case TK_SEMICOLON:
    ++ctx->idx;
    return stmt_init_block(ctx->arena, token->lex, NULL);
  case TK_KW_RETURN: {
    ++ctx->idx;
    Token *next = peek_token(ctx);
    if (next != NULL && next->kind == TK_SEMICOLON) {
      ++ctx->idx;
      return stmt_init_return(ctx->arena, token->lex, NULL);
    }
    ExprNode *expr = parse_expr(ctx, 0);
    expect_token(ctx, TK_SEMICOLON);
    return stmt_init_return(ctx->arena, token->lex, expr);
  }
  case TK_KW_WHILE: {
    ++ctx->idx;
    expect_token(ctx, TK_LPAREN);
    ExprNode *cond = parse_expr(ctx, 0);
    expect_token(ctx, TK_RPAREN);
    StmtNode *body = parse_stmt(ctx);
    return stmt_init_while(ctx->arena, token->lex, cond, body);
  }
  case TK_KW_IF:
    ++ctx->idx;
    expect_token(ctx, TK_LPAREN);
    ExprNode *cond = parse_expr(ctx, 0);
    expect_token(ctx, TK_RPAREN);
    StmtNode *then = parse_stmt(ctx);
    Token *next = peek_token(ctx);
    if (next != NULL && next->kind == TK_KW_ELSE) {
      ++ctx->idx;
      StmtNode *elss = parse_stmt(ctx);
      return stmt_init_if(ctx->arena, token->lex, cond, then, elss);
    }
    return stmt_init_if(ctx->arena, token->lex, cond, then, NULL);
  case TK_KW_FOR: {
    ++ctx->idx;
    expect_token(ctx, TK_LPAREN);
    StmtNode *init = parse_stmt(ctx);
    ExprNode *cond = NULL;
    Token *next = peek_token(ctx);
    if (next != NULL && next->kind != TK_SEMICOLON) {
      cond = parse_expr(ctx, 0);
    }
    expect_token(ctx, TK_SEMICOLON);
    ExprNode *step = NULL;
    next = peek_token(ctx);
    if (next != NULL && next->kind != TK_RPAREN) {
      step = parse_expr(ctx, 0);
    }
    expect_token(ctx, TK_RPAREN);
    StmtNode *body = parse_stmt(ctx);
    return stmt_init_for(ctx->arena, token->lex, init, cond, step, body);
  }
  case TK_KW_VOID:
  case TK_KW_CHAR:
  case TK_KW_SHORT:
  case TK_KW_INT:
  case TK_KW_LONG:
  case TK_KW_FLOAT:
  case TK_KW_DOUBLE:
  case TK_KW_SIGNED:
  case TK_KW_UNSIGNED:
  case TK_KW__BOOL:
  case TK_KW__COMPLEX: {
    ++ctx->idx;
    Token *next = expect_token(ctx, TK_IDENT);
    StringView name = next == NULL ? token->lex : next->lex;
    next = peek_token(ctx);
    if (next != NULL && next->kind != TK_SEMICOLON) {
      expect_token(ctx, TK_EQ);
      ExprNode *expr = parse_expr(ctx, 0);
      expect_token(ctx, TK_SEMICOLON);
      return stmt_init_decl(ctx->arena, token->lex, name, expr);
    }
    expect_token(ctx, TK_SEMICOLON);
    return stmt_init_decl(ctx->arena, token->lex, name, NULL);
  }
  default: {
    ExprNode *expr = parse_expr(ctx, 0);
    token = expect_token(ctx, TK_SEMICOLON);
    token = token != NULL ? token : token_stream_last(ctx->tokens);
    if (expr == NULL) {
      return stmt_init_block(ctx->arena, token->lex, NULL);
    }
    return stmt_init_expr(ctx->arena, token->lex, expr);
  }
  }
}

ParseResult parse(Arena *arena, TokenStream *tokens) {
  ParseResult result = {0};
  ParseCtx ctx = {0, arena, &result, tokens};
  parse_error_stream_init(&result.errors);
  Token *token = peek_token(&ctx);
  if (token->kind != TK_LBRACE) {
    parse_error_stream_push(
        &ctx.result->errors,
        (ParseError){PARSE_ERR_EXPECTED_TOKEN, token, TK_LBRACE});
  }
  result.ast.body = parse_stmt_block(&ctx);
  return result;
}
