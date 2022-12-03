////////////////////////////////////////////////////////////////
// Runtime error support library for ASL's C backend
//
// Copyright Intel Inc (c) 2022
// SPDX-Licence-Identifier: BSD-3-Clause
////////////////////////////////////////////////////////////////

#include "asl/error.h"

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#ifdef __cplusplus
extern "C" {
#endif

void
ASL_error(const char* loc, const char* msg)
{
        fprintf(stderr, "%s: ASL error %s\n\n", loc, msg);
        fprintf(stderr, "This error indicates an error in the specification and should\n");
        fprintf(stderr, "be reported as a bug.\n");

        exit(1);
}

void
runtime_error(const char *msg)
{
        fprintf(stderr, "Runtime error: %s\n", msg);

        exit(1);
}

void
runtime_error_if(bool cond, const char *msg)
{
        if (cond)
                runtime_error(msg);
}

#ifdef __cplusplus
}
#endif

////////////////////////////////////////////////////////////////
// End
////////////////////////////////////////////////////////////////
