#include "scan.h"

#include <ctype.h>
#include <string.h>

DEFINE_VECTOR(ScanError, ScanErrorStream, scan_error_stream)

static inline TokenKind check_keyword(u32 start, u32 size, const char *rest,
                                      StringView *lex, TokenKind kind) {
  if (lex->size == start + size && memcmp(lex->data + start, rest, size) == 0) {
    return kind;
  }
  return TK_ID;
}

static TokenKind lookup_keyword(StringView *lex) {
  switch (lex->data[0]) {
  case '_':
    switch (lex->size) {
    case 5:
      // _Bool
      return check_keyword(1, 4, "Bool", lex, TK_KW__BOOL);
    case 7:
      // _Atomic
      return check_keyword(1, 6, "Atomic", lex, TK_KW__ATOMIC);
    case 8:
      switch (lex->data[1]) {
      case 'A':
        // _Alignas
        return check_keyword(2, 6, "lignas", lex, TK_KW__ALIGNAS);
      case 'C':
        // _Complex
        return check_keyword(2, 6, "omplex", lex, TK_KW__COMPLEX);
      case 'G':
        // _Generic
        return check_keyword(2, 6, "eneric", lex, TK_KW__GENERIC);
      }
      break;
    case 9:
      // _Noreturn
      return check_keyword(1, 8, "Noreturn", lex, TK_KW__NORETURN);
    case 10:
      // _Imaginary
      return check_keyword(1, 9, "Imaginary", lex, TK_KW__IMAGINARY);
    case 13:
      // _Thread_local
      return check_keyword(1, 12, "Thread_local", lex, TK_KW__THREAD_LOCAL);
    case 14:
      // _Static_assert
      return check_keyword(1, 13, "Static_assert", lex, TK_KW__STATIC_ASSERT);
    }
    break;
  case 'a':
    switch (lex->size) {
    case 4:
      // auto
      return check_keyword(1, 3, "uto", lex, TK_KW_AUTO);
    case 7:
      // alignof
      return check_keyword(1, 6, "lignof", lex, TK_KW_ALIGNOF);
    }
    break;
  case 'b':
    // break
    return check_keyword(1, 4, "reak", lex, TK_KW_BREAK);
  case 'c':
    switch (lex->size) {
    case 4:
      switch (lex->data[1]) {
      case 'a':
        // case
        return check_keyword(2, 2, "se", lex, TK_KW_CASE);
      case 'h':
        // char
        return check_keyword(2, 2, "ar", lex, TK_KW_CHAR);
      }
      break;
    case 5:
      // const
      return check_keyword(1, 4, "onst", lex, TK_KW_CONST);
    case 8:
      // continue
      return check_keyword(1, 7, "ontinue", lex, TK_KW_CONTINUE);
    }
    break;
  case 'd':
    switch (lex->size) {
    case 2:
      // do
      return check_keyword(1, 1, "o", lex, TK_KW_DO);
    case 6:
      // double
      return check_keyword(1, 5, "ouble", lex, TK_KW_DOUBLE);
    case 7:
      // default
      return check_keyword(1, 6, "efault", lex, TK_KW_DEFAULT);
    }
    break;
  case 'e':
    switch (lex->size) {
    case 4:
      switch (lex->data[1]) {
      case 'l':
        // else
        return check_keyword(2, 2, "se", lex, TK_KW_ELSE);
      case 'n':
        // enum
        return check_keyword(2, 2, "um", lex, TK_KW_ENUM);
      }
      break;
    case 6:
      // extern
      return check_keyword(1, 5, "xtern", lex, TK_KW_EXTERN);
    }
    break;
  case 'f':
    switch (lex->size) {
    case 3:
      // for
      return check_keyword(1, 2, "or", lex, TK_KW_FOR);
    case 5:
      // float
      return check_keyword(1, 4, "loat", lex, TK_KW_FLOAT);
    }
    break;
  case 'g':
    // goto
    return check_keyword(1, 3, "oto", lex, TK_KW_GOTO);
  case 'i':
    switch (lex->size) {
    case 2:
      // if
      return check_keyword(1, 1, "f", lex, TK_KW_IF);
    case 3:
      // int
      return check_keyword(1, 2, "nt", lex, TK_KW_INT);
    case 6:
      // inline
      return check_keyword(1, 5, "nline", lex, TK_KW_INLINE);
    }
    break;
  case 'l':
    // long
    return check_keyword(1, 3, "ong", lex, TK_KW_LONG);
  case 'r':
    switch (lex->size) {
    case 6:
      // return
      return check_keyword(1, 5, "eturn", lex, TK_KW_RETURN);
    case 8:
      switch (lex->data[1]) {
      case 'e':
        switch (lex->data[2]) {
        case 'g':
          // register
          return check_keyword(3, 5, "ister", lex, TK_KW_REGISTER);
        case 's':
          // restrict
          return check_keyword(3, 5, "trict", lex, TK_KW_RESTRICT);
        }
        break;
      }
      break;
    }
    break;
  case 's':
    switch (lex->size) {
    case 5:
      // short
      return check_keyword(1, 4, "hort", lex, TK_KW_SHORT);
    case 6:
      switch (lex->data[1]) {
      case 'i':
        switch (lex->data[2]) {
        case 'g':
          // signed
          return check_keyword(3, 3, "ned", lex, TK_KW_SIGNED);
        case 'z':
          // sizeof
          return check_keyword(3, 3, "eof", lex, TK_KW_SIZEOF);
        }
        break;
      case 't':
        switch (lex->data[2]) {
        case 'a':
          // static
          return check_keyword(3, 3, "tic", lex, TK_KW_STATIC);
        case 'r':
          // struct
          return check_keyword(3, 3, "uct", lex, TK_KW_STRUCT);
        }
        break;
      case 'w':
        // switch
        return check_keyword(2, 4, "itch", lex, TK_KW_SWITCH);
      }
      break;
    }
    break;
  case 't':
    // typedef
    return check_keyword(1, 6, "ypedef", lex, TK_KW_TYPEDEF);
  case 'u':
    switch (lex->size) {
    case 5:
      // union
      return check_keyword(1, 4, "nion", lex, TK_KW_UNION);
    case 8:
      // unsigned
      return check_keyword(1, 7, "nsigned", lex, TK_KW_UNSIGNED);
    }
    break;
  case 'v':
    switch (lex->size) {
    case 4:
      // void
      return check_keyword(1, 3, "oid", lex, TK_KW_VOID);
    case 8:
      // volatile
      return check_keyword(1, 7, "olatile", lex, TK_KW_VOLATILE);
    }
    break;
  case 'w':
    // while
    return check_keyword(0, 5, "while", lex, TK_KW_WHILE);
  }
  return TK_ID;
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
        token_stream_push(&res.tokens, (Token){lookup_keyword(&lex), lex});
        break;
      } else { // Invalid characters
        scan_error_stream_push(&res.errors, (ScanError){SCAN_ERR_INVALID_CHAR,
                                                        (StringView){c++, 1}});
      }
    }
  }
  return res;
}

void scan_result_free(ScanResult *result) {
  token_stream_free(&result->tokens);
  scan_error_stream_free(&result->errors);
}
