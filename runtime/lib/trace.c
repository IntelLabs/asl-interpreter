////////////////////////////////////////////////////////////////
// Runtime trace support library for ASL's C backend
//
// Copyright Intel Inc (c) 2022
// SPDX-Licence-Identifier: BSD-3-Clause
////////////////////////////////////////////////////////////////

#include "asl/trace.h"

#include <stdint.h>
#include <stdio.h>

#ifdef __cplusplus
extern "C" {
#endif

// count of number of instructions executed
static uint64_t ASL_cycle = 0ULL;

// Advance trace to next instruction
void
__TraceNext_0()
{
        ASL_cycle++;
}

// Emit an error message to trace
void
__TraceError_0(
        const char* kind,
        const char* event)
{
        fprintf(stderr, "TRACE error %ld %s: %s\n", ASL_cycle, kind, event);
}

// Emit an informational memory to trace
void
__TraceEvent_0(
        const char* kind,
        const char* event)
{
        fprintf(stderr, "TRACE event %ld %s: %s\n", ASL_cycle, kind, event);
}

#ifdef __cplusplus
}
#endif

////////////////////////////////////////////////////////////////
// End
////////////////////////////////////////////////////////////////
