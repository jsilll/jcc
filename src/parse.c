#include "parse.h"
#include "ast.h"

void parse_result_free(ParseResult *result) {
  parse_error_vec_free(&result->errors);
}

typedef struct BinOpPair {
  BinOpKind op;
  uint8_t precLHS;
  uint8_t precRHS;
} BinOpPair;

static BinOpPair try_into_binop(TokenKind kind) {
  switch (kind) {
  case TK_EQ:
    return (BinOpPair){BINOP_ASGN, 2, 1};
  case TK_EQ_EQ:
    return (BinOpPair){BINOP_EQ, 3, 4};
  case TK_BANG_EQ:
    return (BinOpPair){BINOP_NE, 3, 4};
  case TK_LT:
    return (BinOpPair){BINOP_LT, 5, 6};
  case TK_LT_EQ:
    return (BinOpPair){BINOP_LE, 5, 6};
  case TK_GT:
    return (BinOpPair){BINOP_GT, 5, 6};
  case TK_GT_EQ:
    return (BinOpPair){BINOP_GE, 5, 6};
  case TK_PLUS:
    return (BinOpPair){BINOP_ADD, 7, 8};
  case TK_MINUS:
    return (BinOpPair){BINOP_SUB, 7, 8};
  case TK_STAR:
    return (BinOpPair){BINOP_MUL, 9, 10};
  case TK_SLASH:
    return (BinOpPair){BINOP_DIV, 9, 10};
  default:
    return (BinOpPair){BINOP_ADD, 0, 0};
  }
}

typedef struct ParseCtx {
  uint32_t idx;
  Arena *arena;
  TokenVec *tokens;
  ParseResult *result;
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
    parse_error_vec_push(
        &ctx->result->errors,
        (ParseError){PARSE_ERR_EXPECTED_SOME, NULL, TK_IDENT});
    return NULL;
  }
  return token;
}

static Token *expect_token(ParseCtx *ctx, TokenKind kind) {
  Token *token = peek_token(ctx);

  if (token == NULL) {
    parse_error_vec_push(&ctx->result->errors,
                            (ParseError){PARSE_ERR_UNEXPECTED_EOF, NULL, kind});
    return NULL;
  }
  
  if (token->kind != kind) {
    parse_error_vec_push(
        &ctx->result->errors,
        (ParseError){PARSE_ERR_EXPECTED_TOKEN, token, kind});
    return NULL;
  }

  ++ctx->idx;
  return token;
}

// TODO: Start using this macro for most error handling
#define RETURN_ON_ERROR(ctx, expr)                                             \
  do {                                                                         \
    if ((expr) == NULL) {                                                      \
      return NULL;                                                             \
    }                                                                          \
  } while (0)

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
    parse_error_vec_push(
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
      if (bp.precLHS <= prec) {
        return lhs;
      }
      ++ctx->idx;
      ExprNode *expr = parse_expr(ctx, bp.precRHS);
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
    Token *prev = peek_token(ctx);
    Token *next = prev;
    while (next != NULL && next->kind != TK_SEMICOLON) {
      DEBUG("Looping");
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
        next = peek_token(ctx);
      } else {
        curr = curr->next =
            stmt_init_decl(ctx->arena, token->lex, type, name, NULL);
      }
      if (prev == next) {
        break;
      }
      prev = next;
    }
    expect_token(ctx, TK_SEMICOLON);
    return head.next;
  }
  default: {
    ExprNode *expr = parse_expr(ctx, 0);
    token = expect_token(ctx, TK_SEMICOLON);
    token = token != NULL ? token : token_vec_last(ctx->tokens);
    if (expr == NULL) {
      return stmt_init_block(ctx->arena, token->lex, NULL);
    }
    return stmt_init_expr(ctx->arena, token->lex, expr);
  }
  }
}

FuncNode *parse_function(ParseCtx *ctx) {
  FuncNode *function = arena_alloc(ctx->arena, sizeof(FuncNode));
  function->type = type_init_func(ctx->arena, NULL, TYPE_INT);
  Type *base = parse_declspec(ctx);
  function->type->u.func.ret = parse_decltype(ctx, base);
  Token *token = expect_token(ctx, TK_IDENT);
  function->lex = token != NULL ? token->lex : EMPTY_SV;
  expect_token(ctx, TK_LPAREN);
  token = peek_some(ctx);
  if (token != NULL && token->kind != TK_RPAREN) {
    FormalArg head = {0};
    FormalArg *curr = &head;
    while (true) {
      Type *type = parse_declspec(ctx);
      type = parse_decltype(ctx, type);
      token = expect_token(ctx, TK_IDENT);
      StringView lex = token != NULL ? token->lex : EMPTY_SV;
      curr = curr->next = arena_alloc(ctx->arena, sizeof(FormalArg));
      curr->decl = stmt_init_decl(ctx->arena, lex, type, lex, NULL);
      token = peek_some(ctx);
      if (token != NULL && token->kind != TK_RPAREN) {
        expect_token(ctx, TK_COMMA);
      } else {
        break;
      }
    }
    function->args = head.next;
  }
  expect_token(ctx, TK_RPAREN);
  token = peek_some(ctx);
  if (token != NULL && token->kind != TK_LBRACE) {
    parse_error_vec_push(
        &ctx->result->errors,
        (ParseError){PARSE_ERR_EXPECTED_TOKEN, token, TK_LBRACE});
  }
  function->body = parse_stmt_block(ctx);
  TypeList head = {0};
  TypeList *curr = &head;
  for (FormalArg *arg = function->args; arg != NULL; arg = arg->next) {
    curr = curr->next = arena_alloc(ctx->arena, sizeof(TypeList));
    curr->type = arg->decl->u.decl.type;
  }
  function->type->u.func.args = head.next;
  return function;
}

ParseResult parse(Arena *arena, TokenVec *tokens) {
  ParseResult result = {0};
  parse_error_vec_init(&result.errors);
  ParseCtx ctx = {0, arena, tokens, &result};

  FuncNode head = {0};
  FuncNode *curr = &head;

  while (true) {
    Token *token = peek_token(&ctx);
    if (token == NULL) {
      break;
    }

    curr->next = parse_function(&ctx);
    curr = curr->next;
  }

  result.ast = head.next;

  Token *remaining = peek_token(&ctx);
  if (remaining != NULL) {
    parse_error_vec_push(&result.errors, (ParseError){PARSE_ERR_EXPECTED_EOF,
                                                         remaining, TK_IDENT});
  }

  return result;
}
