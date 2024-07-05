#include "codegen_x86.h"
#include "desugar.h"
#include "error.h"
#include "parse.h"
#include "resolve.h"
#include "scan.h"
#include "sema.h"

static void print_usage(const char *name) {
  fprintf(stderr, "usage: %s [ <stmt> | --file <file> ] [--emit-tokens]\n",
          name);
}

static void report_file_errors(const char *name, const FileResult result) {
  switch (result) {
  case FILE_SUCCESS:
    PANIC("unexpected file result");
    break;
  case FILE_ERR_EMPTY:
    fprintf(stderr, "error: file '%s' not found\n", name);
    break;
  case FILE_ERR_OPEN:
    fprintf(stderr, "error: failed to open file '%s'\n", name);
    break;
  case FILE_ERR_READ:
    fprintf(stderr, "error: failed to read file '%s'\n", name);
    break;
  }
}

static void report_scan_errors(const SrcFile *file,
                               const ScanErrorStream *errors, uint32_t max) {
  max = MIN(max, errors->size);
  for (uint32_t i = 0; i < max; ++i) {
    switch (errors->data[i].kind) {
    case SCAN_ERR_INVALID_CHAR:
      if (IS_ASCII(*errors->data[i].lex.data)) {
        error_at(file, errors->data[i].lex, "unexpected character",
                 "character '%c' is not allowed", *errors->data[i].lex.data);
      } else {
        error_at(file, errors->data[i].lex, "unexpected character",
                 "character is not an ASCII");
      }
      break;
    case SCAN_ERR_INVALID_ESCAPE:
      error_at(file, errors->data[i].lex, "invalid escape sequence",
               "escape sequence is invalid");
      break;
    case SCAN_ERR_INVALID_SEQUENCE:
      error_at(file, errors->data[i].lex, "invalid sequence",
               "this sequence of characters does not match any token");
      break;
    case SCAN_ERR_UNTERMINATED_CHAR:
      error_at(file, errors->data[i].lex, "unterminated character literal",
               "character literal is not terminated");
      break;
    case SCAN_ERR_UNTERMINATED_STRING:
      error_at(file, errors->data[i].lex, "unterminated string literal",
               "string literal is not terminated");
      break;
    }
  }
}

static void report_parse_errors(const SrcFile *file,
                                const ParseErrorStream *errors, uint32_t max) {
  max = MIN(max, errors->size);
  for (uint32_t i = 0; i < max; ++i) {
    switch (errors->data[i].kind) {
    case PARSE_ERR_EXPECTED_SOME:
      error_at(file, (StringView){file->end, 1}, "unexpected end of file",
               "expected some token instead");
      break;
    case PARSE_ERR_EXPECTED_TOKEN:
      error_at(file, errors->data[i].token->lex, "unexpected token",
               "expected %s instead", token_kind_lex(errors->data[i].expected));
      break;
    case PARSE_ERR_UNEXPECTED_EOF:
      error_at(file, (StringView){file->end, 1}, "unexpected end of file",
               "expected %s instead", token_kind_lex(errors->data[i].expected));
      break;
    case PARSE_ERR_UNEXPECTED_TOKEN:
      error_at(file, errors->data[i].token->lex, "unexpected token",
               "expected some other token instead");
      break;
    }
  }
}

static void report_resolve_errors(const SrcFile *file,
                                  const ResolveErrorStream *errors,
                                  uint32_t max) {
  max = MIN(max, errors->size);
  for (uint32_t i = 0; i < max; ++i) {
    switch (errors->data[i].kind) {
    case RESOLVE_ERR_DUPLICATE:
      error_at(file, errors->data[i].span, "duplicate declaration",
               "declaration already exists");
      break;
    case RESOLVE_ERR_UNDECLARED:
      error_at(file, errors->data[i].span, "undeclared identifier",
               "must be declared before usage");
      break;
    }
  }
}

static void report_sema_errors(const SrcFile *file,
                               const SemaErrorStream *errors, uint32_t max) {
  max = MIN(max, errors->size);
  for (uint32_t i = 0; i < max; ++i) {
    switch (errors->data[i].kind) {
    case SEMA_ERR_MISMATCHED_TYPES:
      error_at(file, errors->data[i].span, "mismatched types",
               "types are mismatched");
      break;
    }
  }
}

int main(int argc, char *argv[]) {
  if (argc == 1 || argc > 4) {
    print_usage(argv[0]);
    return EXIT_FAILURE;
  }

  SrcFile file;
  bool emit_tokens = false;
  bool emit_ast = false;
  if (argc == 2) {
    src_file_init_from_raw(&file, "stdin", argv[1]);
  } else if (strcmp(argv[1], "--file") == 0) {
    FileResult fr = src_file_init(&file, argv[2]);
    if (fr != FILE_SUCCESS) {
      report_file_errors(argv[2], fr);
      return EXIT_FAILURE;
    }
    for (int i = 3; i < argc; ++i) {
      if (strcmp(argv[i], "--emit-tokens") == 0) {
        emit_tokens = true;
      } else if (strcmp(argv[i], "--emit-ast") == 0) {
        emit_ast = true;
      }
    }
  } else {
    src_file_init_from_raw(&file, "stdin", argv[1]);
    for (int i = 2; i < argc; ++i) {
      if (strcmp(argv[i], "--emit-tokens") == 0) {
        emit_tokens = true;
      } else if (strcmp(argv[i], "--emit-ast") == 0) {
        emit_ast = true;
      }
    }
  }

  FuncNode ast = {0};
  Arena arena;
  arena_init(&arena, KB(64));

  {
    /// Scan  + Parse ///
    DEBUG("Scan");
    ScanResult sr = scan(&file, true);
    if (emit_tokens) {
      fprintf(stderr, "TOKENS\n");
      token_stream_debug(stderr, &sr.tokens, &file);
    }
    if (sr.errors.size > 0) {
      report_scan_errors(&file, &sr.errors, 1);

      scan_result_free(&sr);
      src_file_free(&file);
      return EXIT_FAILURE;
    }

    DEBUG("Parse");
    ParseResult pr = parse(&arena, &sr.tokens);
    if (emit_ast) {
      fprintf(stderr, "AST\n");
      ast_debug(stderr, &pr.ast);
    }
    if (pr.errors.size > 0) {
      report_parse_errors(&file, &pr.errors, pr.errors.size);

      parse_result_free(&pr);
      arena_free(&arena);
      scan_result_free(&sr);
      src_file_free(&file);
      return EXIT_FAILURE;
    }

    ast = pr.ast;

    parse_result_free(&pr);
    scan_result_free(&sr);

    DEBUGF("arena total blocks: %d", arena_total_blocks(&arena));
    DEBUGF("arena commited bytes: %zu", arena_commited_bytes(&arena));
  }

  {
    /// Resolve ///
    DEBUG("Resolve");
    ResolveResult rr = resolve(&ast);
    if (emit_ast) {
      fprintf(stderr, "RESOLVED AST\n");
      ast_debug(stderr, &ast);
    }
    if (rr.errors.size > 0) {
      report_resolve_errors(&file, &rr.errors, rr.errors.size);

      resolve_result_free(&rr);
      arena_free(&arena);
      src_file_free(&file);
      return EXIT_FAILURE;
    }

    resolve_result_free(&rr);
  }

  /// Sema ///
  {
    DEBUG("Sema");
    SemaResult smr = sema(&arena, &ast);
    if (emit_ast) {
      fprintf(stderr, "SEMA AST\n");
      ast_debug(stderr, &ast);
    }
    if (smr.errors.size > 0) {
      report_sema_errors(&file, &smr.errors, smr.errors.size);

      sema_result_free(&smr);
      arena_free(&arena);
      src_file_free(&file);
      return EXIT_FAILURE;
    }

    sema_result_free(&smr);

    DEBUGF("arena total blocks: %d", arena_total_blocks(&arena));
    DEBUGF("arena commited bytes: %zu", arena_commited_bytes(&arena));
  }

  /// Desugar ///
  DEBUG("Desugar");
  desugar(&arena, &ast);
  if (emit_ast) {
    fprintf(stderr, "DESUGARED AST\n");
    ast_debug(stderr, &ast);
  }

  DEBUGF("arena total blocks: %d", arena_total_blocks(&arena));
  DEBUGF("arena commited bytes: %zu", arena_commited_bytes(&arena));

  /// Codegen ///
  codegen_x86(stdout, &ast);

  arena_free(&arena);
  src_file_free(&file);
  return EXIT_SUCCESS;
}
