#include "codegen_x86.h"
#include "desugar.h"
#include "parse.h"
#include "resolve.h"
#include "scan.h"
#include "sema.h"

#include "support/base.h"
#include "support/error.h"
#include "support/file.h"

#include <stdlib.h>

/// Command Line Options ///

typedef struct CliOptions {
  bool emit_tokens;
  bool emit_ast;
  char *filename;
} CliOptions;

static void print_usage(const char *name) {
  fprintf(
      stderr,
      "usage: %s [ <stmt> | --file <file> ] [--emit-tokens | --emit-ast ]\n",
      name);
}

static bool parse_cli_options(int argc, char *argv[], CliOptions *options) {
  if (argc == 1 || argc > 4) {
    print_usage(argv[0]);
    return false;
  }

  if (argc == 2) {
    options->filename = "stdin";
  } else if (strcmp(argv[1], "--file") == 0) {
    options->filename = argv[2];
    for (int i = 3; i < argc; ++i) {
      if (strcmp(argv[i], "--emit-tokens") == 0) {
        options->emit_tokens = true;
      } else if (strcmp(argv[i], "--emit-ast") == 0) {
        options->emit_ast = true;
      }
    }
  } else {
    options->filename = "stdin";
    for (int i = 2; i < argc; ++i) {
      if (strcmp(argv[i], "--emit-tokens") == 0) {
        options->emit_tokens = true;
      } else if (strcmp(argv[i], "--emit-ast") == 0) {
        options->emit_ast = true;
      }
    }
  }

  return true;
}

/// File IO ///

static void report_file_error(const char *name, const FileResult result) {
  switch (result) {
  case FILE_SUCCESS:
    PANIC("unexpected file result");
    break;
  case FILE_ERR_EMPTY:
    error("file '%s' not found\n", name);
    break;
  case FILE_ERR_OPEN:
    error("failed to open file '%s'\n", name);
    break;
  case FILE_ERR_READ:
    error("failed to read file '%s'\n", name);
    break;
  }
}

/// Scan ///

static void report_scan_errors(const SrcFile *file, const ScanErrorVec *errors,
                               size_t max) {
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

/// Parse ///

static void report_parse_errors(const SrcFile *file,
                                const ParseErrorVec *errors, size_t max) {
  max = MIN(max, errors->size);
  for (uint32_t i = 0; i < max; ++i) {
    switch (errors->data[i].kind) {
    case PARSE_ERR_EXPECTED_EOF:
      error_at(file, errors->data[i].token->lex, "unexpected token",
               "expected end of file instead");
      break;
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

static FuncNode *parse_driver(const CliOptions *options, const SrcFile *file,
                              Arena *arena) {
  DEBUG("Scan");
  ScanResult sr = scan(file, true);
  if (options->emit_tokens) {
    fprintf(stderr, "== TOKENS ==\n");
    token_stream_debug(stderr, &sr.tokens, file);
  }
  if (sr.errors.size > 0) {
    report_scan_errors(file, &sr.errors, 1);
    scan_result_free(&sr);
    return NULL;
  }
  DEBUGF("sr.tokens.capacity: %zu", sr.tokens.capacity);
  DEBUG("Parse");
  ParseResult pr = parse(arena, &sr.tokens);
  if (options->emit_ast) {
    fprintf(stderr, "== AST ==\n");
    ast_debug(stderr, pr.ast);
  }
  if (pr.errors.size > 0) {
    report_parse_errors(file, &pr.errors, 1);
    pr.ast = NULL;
  } else if (pr.ast == NULL) {
    error("empty parse tree");
  }
  parse_result_free(&pr);
  scan_result_free(&sr);
  DEBUGF("arena total blocks: %d", arena_total_blocks(arena));
  DEBUGF("arena commited bytes: %zu", arena_commited_bytes(arena));
  return pr.ast;
}

/// Resolve ///

static void report_resolve_errors(const SrcFile *file,
                                  const ResolveErrorVec *errors, size_t max) {
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

static bool resolve_driver(const CliOptions *options, const SrcFile *file,
                           FuncNode *ast) {
  DEBUG("Resolve");
  ResolveResult rr = resolve(ast);
  if (options->emit_ast) {
    fprintf(stderr, "== RESOLVED AST ==\n");
    ast_debug(stderr, ast);
  }
  bool success = true;
  if (rr.errors.size > 0) {
    report_resolve_errors(file, &rr.errors, rr.errors.size);
    success = false;
  }
  resolve_result_free(&rr);
  return success;
}

/// Sema ///

static void report_sema_errors(const SrcFile *file, const SemaErrorVec *errors,
                               size_t max) {
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

static bool sema_driver(const CliOptions *options, const SrcFile *file,
                        Arena *arena, FuncNode *ast) {
  DEBUG("Sema");
  SemaResult smr = sema(arena, ast);
  if (options->emit_ast) {
    fprintf(stderr, "== SEMA AST ==\n");
    ast_debug(stderr, ast);
  }
  bool success = true;
  if (smr.errors.size > 0) {
    report_sema_errors(file, &smr.errors, smr.errors.size);
    success = false;
  }
  sema_result_free(&smr);
  DEBUGF("arena total blocks: %d", arena_total_blocks(arena));
  DEBUGF("arena commited bytes: %zu", arena_commited_bytes(arena));
  return success;
}

/// Desugar ///

static bool desugar_driver(const CliOptions *options, Arena *arena,
                           FuncNode *ast) {
  DEBUG("Desugar");
  desugar(arena, ast);
  if (options->emit_ast) {
    fprintf(stderr, "== DESUGARED AST ==\n");
    ast_debug(stderr, ast);
  }
  DEBUGF("arena total blocks: %d", arena_total_blocks(arena));
  DEBUGF("arena commited bytes: %zu", arena_commited_bytes(arena));
  return true;
}

/// Codegen ///

static bool codegen_driver(FuncNode *ast) {
  DEBUG("Codegen");
  codegen_x86(stdout, ast);
  return true;
}

/// Main ///

int main(int argc, char *argv[]) {
  CliOptions options = {0};
  if (!parse_cli_options(argc, argv, &options)) {
    return EXIT_FAILURE;
  }

  SrcFile file = {0};
  if (strcmp(options.filename, "stdin") == 0) {
    sf_from_raw(&file, "stdin", argv[1]);
  } else {
    FileResult fr = sf_init(&file, options.filename);
    if (fr != FILE_SUCCESS) {
      report_file_error(options.filename, fr);
      return EXIT_FAILURE;
    }
  }

  Arena arena = {0};
  arena_init(&arena, KB(64));

  FuncNode *ast = parse_driver(&options, &file, &arena);
  if (ast == NULL) {
    arena_free(&arena);
    sf_free(&file);
    return EXIT_FAILURE;
  }

  if (!resolve_driver(&options, &file, ast)) {
    arena_free(&arena);
    sf_free(&file);
    return EXIT_FAILURE;
  }

  if (!sema_driver(&options, &file, &arena, ast)) {
    arena_free(&arena);
    sf_free(&file);
    return EXIT_FAILURE;
  }

  if (!desugar_driver(&options, &arena, ast)) {
    arena_free(&arena);
    sf_free(&file);
    return EXIT_FAILURE;
  }

  if (!codegen_driver(ast)) {
    arena_free(&arena);
    sf_free(&file);
    return EXIT_FAILURE;
  }

  arena_free(&arena);
  sf_free(&file);
}
