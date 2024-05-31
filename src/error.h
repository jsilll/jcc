#ifndef CC_ERROR_H
#define CC_ERROR_H

#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>

/// Print an error message and exit.
/// @param fmt The format string.
/// @param ... The arguments to the format string.
void error(const char *fmt, ...);

/// Print an error message with a location and exit.
/// @param start The start of the source code.
/// @param loc The location of the error.
/// @param fmt The format string.
void error_at(const char *start, const char *loc, const char *fmt, ...);

#endif // CC_ERROR_H
