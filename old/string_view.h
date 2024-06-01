#ifndef CC_STRING_VIEW_H
#define CC_STRING_VIEW_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

/// A view of a string
typedef struct {
  const char *ptr;
  uint32_t len;
} StringView;

/// Create a new string view
/// @param str The string to view
/// @param len The length of the string
StringView string_view_create(const char *str, size_t len);

/// Split a string view by a delimiter
/// @param sv The string view to split
/// @param delim The delimiter to split by
StringView string_view_split(StringView sv, char delim);

/// Check whether a string view is empty
/// @param sv The string view to check
bool string_view_empty(StringView sv);

/// Check whether two string views are equal
/// @param sv1 The first string view
/// @param sv2 The second string view
bool string_view_equals(StringView sv1, StringView sv2);

/// Check whether a string view contains a substring
/// @param sv The string view to search
/// @param substr The substring to search for
bool string_view_contains(StringView sv, StringView substr);

/// Check whether a string view starts with a prefix
/// @param sv The string view to search
/// @param prefix The prefix to search for
bool string_view_starts_with(StringView sv, StringView prefix);

/// Check whether a string view ends with a suffix
/// @param sv The string view to search
/// @param suffix The suffix to search for
bool string_view_ends_with(StringView sv, StringView suffix);

/// Find the first occurrence of a substring in a string view
/// @param sv The string view to search
/// @param substr The substring to search for
size_t string_view_find(StringView sv, StringView substr);

/// Find the last occurrence of a substring in a string view
/// @param sv The string view to search
/// @param substr The substring to search for
size_t string_view_rfind(StringView sv, StringView substr);

#endif // CC_STRING_VIEW_H
