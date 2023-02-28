////////////////////////////////////////////////////////////////
// Runtime bitvector support library for ASL's C backend
//
// Copyright Intel Inc (c) 2023
// SPDX-Licence-Identifier: BSD-3-Clause
////////////////////////////////////////////////////////////////

#include "asl/bits64.h"

#include <stdbool.h>
#include <stdint.h>

ASL_bits64_t
ASL_and_bits_64(int width, ASL_bits64_t x, ASL_bits64_t y)
{
        return x & y;
}

ASL_bits64_t
ASL_eor_bits_64(int width, ASL_bits64_t x, ASL_bits64_t y)
{
        return x ^ y;
}

bool
ASL_eq_bits_64(int width, ASL_bits64_t x, ASL_bits64_t y)
{
        return x == y;
}

ASL_bits64_t
ASL_lsl_bits_64(int width, ASL_bits64_t x, int d)
{
        return (x << d) & ASL_mk_mask_64(width);
}

ASL_bits64_t
ASL_lsr_bits_64(int width, ASL_bits64_t x, int d)
{
        return x >> d;
}

ASL_bits64_t
ASL_mk_mask_64(int w)
{
        return w < 64 ? (1ULL << w) - 1 : UINT64_MAX;
}

bool
ASL_ne_bits_64(int width, ASL_bits64_t x, ASL_bits64_t y)
{
        return x != y;
}

ASL_bits64_t
ASL_not_bits_64(int width, ASL_bits64_t x)
{
        return ~x & ASL_mk_mask_64(width);
}

ASL_bits64_t
ASL_or_bits_64(int width, ASL_bits64_t x, ASL_bits64_t y)
{
        return x | y;
}

////////////////////////////////////////////////////////////////
// End
////////////////////////////////////////////////////////////////
