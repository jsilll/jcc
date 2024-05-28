#include "string_view.h"

#include <string.h>

StringView string_view_create(char *str, size_t len) {
  return (StringView){.ptr = str, .len = len};
}

StringView string_view_split(StringView sv, char delim) {
  for (size_t i = 0; i < sv.len; i++) {
    if (sv.ptr[i] == delim) {
      return string_view_create(sv.ptr, i);
    }
  }
  return sv;
}

bool string_view_empty(StringView sv) { return sv.len == 0; }

bool string_view_equals(StringView sv1, StringView sv2) {
  if (sv1.len != sv2.len) {
    return false;
  }
  return memcmp(sv1.ptr, sv2.ptr, sv1.len) == 0;
}

bool string_view_contains(StringView sv, StringView substr) {
  if (sv.len < substr.len) {
    return false;
  }
  for (size_t i = 0; i <= sv.len - substr.len; i++) {
    if (memcmp(sv.ptr + i, substr.ptr, substr.len) == 0) {
      return true;
    }
  }
  return false;
}

bool string_view_starts_with(StringView sv, StringView prefix) {
  if (sv.len < prefix.len) {
    return false;
  }
  return memcmp(sv.ptr, prefix.ptr, prefix.len) == 0;
}

bool string_view_ends_with(StringView sv, StringView suffix) {
  if (sv.len < suffix.len) {
    return false;
  }
  return memcmp(sv.ptr + sv.len - suffix.len, suffix.ptr, suffix.len) == 0;
}

size_t string_view_find(StringView sv, StringView substr) {
  for (size_t i = 0; i <= sv.len - substr.len; i++) {
    if (memcmp(sv.ptr + i, substr.ptr, substr.len) == 0) {
      return i;
    }
  }
  return sv.len;
}

size_t string_view_rfind(StringView sv, StringView substr) {
  for (size_t i = sv.len - substr.len; i > 0; i--) {
    if (memcmp(sv.ptr + i, substr.ptr, substr.len) == 0) {
      return i;
    }
  }
  return sv.len;
}
