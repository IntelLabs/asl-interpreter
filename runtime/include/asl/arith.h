////////////////////////////////////////////////////////////////
// Arithmetic runtime library for ASL's C backend
//
// Copyright Intel Inc (c) 2022
// SPDX-Licence-Identifier: BSD-3-Clause
////////////////////////////////////////////////////////////////

#ifndef ASL_ARITH_H
#define ASL_ARITH_H

#include <stdbool.h>
#include <stdint.h>

#include "asl/integer.h"

#ifdef __cplusplus
extern "C" {
#endif

static inline bool
ASL_is_pow2_int(ASL_int_t x)
{
        return x != 0 && (x & (x - 1)) == 0;
}

static inline ASL_int_t
ASL_fdiv_int(ASL_int_t x, ASL_int_t y)
{
        const ASL_int_t quot = x / y;
        const ASL_int_t rem = x % y;
        return quot - (rem != 0 && quot < 0);
}

static inline ASL_int_t
ASL_frem_int(ASL_int_t x, ASL_int_t y)
{
        return x - ASL_fdiv_int(x, y) * y;
}

static inline ASL_int_t
ASL_mask_int(ASL_int_t w)
{
#ifdef ASL_INT128
        return (unsigned __int128)(-1LL) >> (128 - w);
#else
        return UINT64_MAX >> (64 - w);
#endif
}

#ifdef __cplusplus
}
#endif

#endif  // ASL_ARITH_H

////////////////////////////////////////////////////////////////
// End
////////////////////////////////////////////////////////////////
