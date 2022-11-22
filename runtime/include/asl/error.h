////////////////////////////////////////////////////////////////
// Runtime error support library for ASL's C backend
//
// Copyright Intel Inc (c) 2022
// SPDX-Licence-Identifier: BSD-3-Clause
////////////////////////////////////////////////////////////////

#ifndef ASL_ERROR_H
#define ASL_ERROR_H

#ifdef __cplusplus
extern "C" {
#endif

void ASL_error(const char* loc, const char* msg) __attribute__((__noreturn__));

#define ASL_error_unmatched_case(loc) ASL_error(loc, "Unmatched case statement")

#ifdef __cplusplus
}
#endif

#endif  // ASL_ERROR_H

////////////////////////////////////////////////////////////////
// End
////////////////////////////////////////////////////////////////
