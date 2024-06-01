#include "scan.h"

#include <ctype.h>

DEFINE_VECTOR(ScanError, ScanErrorStream, scan_error_stream)

void scan_result_free(ScanResult *result) {
  token_stream_free(&result->tokens);
  scan_error_stream_free(&result->errors);
}

ScanResult scan(File *file) {
  ScanResult res;
  token_stream_init(&res.tokens);
  scan_error_stream_init(&res.errors);
  char *c = file->data;
  while (c != file->end) {
    while (c != file->end && (isspace(*c) || *c == '\0')) {
      ++c;
    }
    if (c == file->end) {
      break;
    }
    switch (*c) {
    case '"': // String literals
      TODO("Scan string literals");
      break;
    case '\'': // Character literals
      TODO("Scan character literals");
      break;
    default:
      if (isdigit(*c)) { // Integer literals
        StringView lex = {c, 1};
        while (++c != file->end && isdigit(*c)) {
          ++lex.size;
        }
        token_stream_push(&res.tokens, (Token){TK_INT, lex});
        break;
      } else if (isalpha(*c) || *c == '_') { // Identifiers
        StringView lex = {c, 1};
        while (++c != file->end && (isalnum(*c) || *c == '_')) {
          ++lex.size;
        }
        token_stream_push(&res.tokens, (Token){TK_ID, lex});
        break;
      } else { // Invalid characters
        scan_error_stream_push(&res.errors, (ScanError){SCAN_ERR_INVALID_CHAR,
                                                        (StringView){c++, 1}});
      }
    }
  }
  return res;
}
