////////////////////////////////////////////////////////////////
// Runtime library for ASL's CPP backend
//
// Copyright (C) 2024 Intel Corporation
// SPDX-Licence-Identifier: BSD-3-Clause
////////////////////////////////////////////////////////////////

#pragma once

#include "asl/error.h"
#include <ac_int.h>

namespace asl
{
    template <int T, int SIZE_IN=1, bool S = false>
    inline ac_int<T*SIZE_IN, S> replicate(int n, const ac_int<SIZE_IN, S> &x)
    {
        ac_int<T*SIZE_IN, S> r = 0;
        while (n-- > 0)
                r = (r << SIZE_IN) | x;
        return r;
    }
}

////////////////////////////////////////////////////////////////
// End
////////////////////////////////////////////////////////////////
