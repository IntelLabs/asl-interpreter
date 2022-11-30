////////////////////////////////////////////////////////////////
// Runtime error support library for ASL's C backend
//
// Copyright Intel Inc (c) 2022
// SPDX-Licence-Identifier: BSD-3-Clause
////////////////////////////////////////////////////////////////

#include "asl/runtime.h"

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

#ifdef __cplusplus
}
#endif

////////////////////////////////////////////////////////////////
// End
////////////////////////////////////////////////////////////////
