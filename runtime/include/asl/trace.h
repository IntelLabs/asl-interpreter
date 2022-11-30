////////////////////////////////////////////////////////////////
// Runtime trace support library for ASL's C backend
//
// Copyright Intel Inc (c) 2022
// SPDX-Licence-Identifier: BSD-3-Clause
////////////////////////////////////////////////////////////////

#ifndef ASL_TRACE_H
#define ASL_TRACE_H

#ifdef __cplusplus
extern "C" {
#endif

// Advance trace to next instruction
void
__TraceNext_0();

// Emit an error message to trace
void
__TraceError_0(
        const char* kind,
        const char* event);

// Emit an informational memory to trace
void
__TraceEvent_0(
        const char* kind,
        const char* event);

#ifdef __cplusplus
}
#endif

#endif  // ASL_TRACE_H

////////////////////////////////////////////////////////////////
// End
////////////////////////////////////////////////////////////////
