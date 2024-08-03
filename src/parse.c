#include "parse.h"

DEFINE_VECTOR(ParseError, ParseErrorStream, parse_error_stream)

void parse_result_free(ParseResult *result) {
  parse_error_stream_free(&result->errors);
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
    return expr_init_int(ctx->arena, token->lex);
  case TK_IDENT:
    return expr_init_var(ctx->arena, token->lex);
  case TK_PLUS:
    return expr_init_unary(ctx->arena, token->lex, UNOP_ADD,
                           parse_expr(ctx, 99));
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
        (ParseError){PARSE_ERR_UNEXPECTED_TOKEN, token, TK_IDENT});
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
    switch (token->kind) {
    case TK_LBRACK: {
      ++ctx->idx;
      ExprNode *idx = parse_expr(ctx, 0);
      expect_token(ctx, TK_LBRACK);
      lhs = expr_init_index(ctx->arena, token->lex, lhs, idx);
    } break;
    case TK_LPAREN: {
      ++ctx->idx;
      ActualArg head = {0};
      ActualArg *curr = &head;
      Token *next = peek_some(ctx);
      while (next != NULL && next->kind != TK_RPAREN) {
        curr->next = arena_alloc(ctx->arena, sizeof(ActualArg));
        curr = curr->next;
        curr->expr = parse_expr(ctx, 0);
        next = peek_some(ctx);
        if (next != NULL && next->kind != TK_RPAREN) {
          expect_token(ctx, TK_COMMA);
          next = peek_some(ctx);
        }
      }
      expect_token(ctx, TK_RPAREN);
      lhs = expr_init_call(ctx->arena, token->lex, lhs, head.next);
    } break;
    default: {
      BinOpPair bp = try_into_binop(token->kind);
      if (bp.bind.lhs <= prec) {
        return lhs;
      }
      ++ctx->idx;
      ExprNode *expr = parse_expr(ctx, bp.bind.rhs);
      lhs = expr_init_binary(ctx->arena, token->lex, bp.op, lhs, expr);
    }
    }
  }
  return lhs;
}

static Type *parse_declspec(ParseCtx *ctx) {
  expect_token(ctx, TK_KW_INT);
  return TYPE_INT;
}

static Type *parse_decltype(ParseCtx *ctx, Type *base) {
  Token *next = peek_token(ctx);
  while (next != NULL && next->kind == TK_STAR) {
    base = type_init_ptr(ctx->arena, base);
    ++ctx->idx;
    next = peek_token(ctx);
  }
  return base;
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
    while (curr->next != NULL) {
      curr = curr->next;
    }
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
  case TK_KW_INT: {
    Type *base = parse_declspec(ctx);
    uint32_t idx = 0;
    StmtNode head = {0};
    StmtNode *curr = &head;
    Token *next = peek_token(ctx);
    while (next != NULL && next->kind != TK_SEMICOLON) {
      if (idx++ > 0) {
        expect_token(ctx, TK_COMMA);
      }
      Type *type = parse_decltype(ctx, base);
      next = expect_token(ctx, TK_IDENT);
      StringView name = next == NULL ? token->lex : next->lex;
      next = peek_token(ctx);
      if (next != NULL && next->kind == TK_EQ) {
        ++ctx->idx;
        ExprNode *expr = parse_expr(ctx, 0);
        curr = curr->next =
            stmt_init_decl(ctx->arena, token->lex, type, name, expr);
      } else {
        curr = curr->next =
            stmt_init_decl(ctx->arena, token->lex, type, name, NULL);
      }
      next = peek_token(ctx);
    }
    expect_token(ctx, TK_SEMICOLON);
    return head.next;
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

FuncNode *parse_function(ParseCtx *ctx) {
  FuncNode *function = arena_alloc(ctx->arena, sizeof(FuncNode));
  Type *base = parse_declspec(ctx);
  function->type = parse_decltype(ctx, base);
  Token *token = expect_token(ctx, TK_IDENT);
  function->lex = token != NULL ? token->lex : EMPTY_SV;
  expect_token(ctx, TK_LPAREN);
  expect_token(ctx, TK_RPAREN);
  token = peek_some(ctx);
  if (token != NULL && token->kind != TK_LBRACE) {
    parse_error_stream_push(
        &ctx->result->errors,
        (ParseError){PARSE_ERR_EXPECTED_TOKEN, token, TK_LBRACE});
  }
  function->body = parse_stmt_block(ctx);
  return function;
}

ParseResult parse(Arena *arena, TokenStream *tokens) {
  ParseResult result = {0};
  ParseCtx ctx = {0, arena, &result, tokens};
  parse_error_stream_init(&result.errors);
  Token *first = peek_token(&ctx);
  if (first != NULL) {
    result.ast = parse_function(&ctx);
  }
  return result;
}
