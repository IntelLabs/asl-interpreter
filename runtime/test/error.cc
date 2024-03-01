////////////////////////////////////////////////////////////////
// Functions defined outside of C runtime error support library
//
// Copyright (C) 2023-2024 Intel Corporation
// SPDX-Licence-Identifier: BSD-3-Clause
////////////////////////////////////////////////////////////////

#include "asl/error.h"

#include <stdio.h>
#include <stdlib.h>

void
ASL_error(const char* loc, const char* msg)
{
        fprintf(stderr, "%s: ASL error %s\n\n", loc, msg);
        exit(1);
}

void
runtime_error(const char *msg)
{
        fprintf(stderr, "Runtime error: %s\n", msg);
        exit(1);
}
