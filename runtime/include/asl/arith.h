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

#ifdef __cplusplus
extern "C" {
#endif

static inline bool
ASL_is_pow2_int(uint64_t x)
{
        return x != 0 && (x & (x - 1)) == 0;
}

static inline int64_t
ASL_fdiv_int(int64_t x, int64_t y)
{
        const int64_t quot = x / y;
        const int64_t rem = x % y;
        return quot - (rem != 0 && quot < 0);
}

static inline int64_t
ASL_frem_int(int64_t x, int64_t y)
{
        return x - ASL_fdiv_int(x, y) * y;
}

#ifdef __cplusplus
}
#endif

#endif  // ASL_ARITH_H

////////////////////////////////////////////////////////////////
// End
////////////////////////////////////////////////////////////////
