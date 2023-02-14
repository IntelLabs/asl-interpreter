////////////////////////////////////////////////////////////////
// Runtime error support library for ASL's C backend
//
// Copyright Intel Inc (c) 2022
// SPDX-Licence-Identifier: BSD-3-Clause
////////////////////////////////////////////////////////////////

#ifndef ASL_ERROR_H
#define ASL_ERROR_H

#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

#define NORETURN __attribute__((__noreturn__))

NORETURN void ASL_error(const char *loc, const char *msg);

#define ASL_error_unmatched_case(loc) ASL_error(loc, "Unmatched case statement")

NORETURN void runtime_error(const char *msg);
void runtime_error_if(bool cond, const char *msg);

void ASL_assert(const char* loc, const char* expr, bool c);

#ifdef __cplusplus
}
#endif

#endif  // ASL_ERROR_H

////////////////////////////////////////////////////////////////
// End
////////////////////////////////////////////////////////////////
