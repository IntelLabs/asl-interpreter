////////////////////////////////////////////////////////////////
// Runtime bitvector support library for ASL's C backend
//
// Copyright Intel Inc (c) 2023
// SPDX-Licence-Identifier: BSD-3-Clause
////////////////////////////////////////////////////////////////

#include "asl/bits64.h"

#include <stdbool.h>
#include <stdint.h>

#include "asl/integer.h"

ASL_bits64_t
ASL_add_bits_64(int width, ASL_bits64_t x, ASL_bits64_t y)
{
        return (x + y) & ASL_mk_mask_64(width);
}

ASL_bits64_t
ASL_and_bits_64(int width, ASL_bits64_t x, ASL_bits64_t y)
{
        return x & y;
}

ASL_bits64_t
ASL_asr_bits_64(int width, ASL_bits64_t x, int d)
{
        bool sign_bit = x >> (width - 1);
        if (sign_bit) {
                x = ASL_not_bits_64(width, x);
                x = ASL_lsr_bits_64(width, x, d);
                x = ASL_not_bits_64(width, x);
        } else {
                x = ASL_lsr_bits_64(width, x, d);
        }
        return x;
}

ASL_int64_t
ASL_cvt_bits_sint_64(int width, ASL_bits64_t x)
{
        const uint64_t mask = 1ULL << (width - 1);
        /* If the sign bit is 1 then, after XOR-ing,
           the subtraction borrows from higher bits making them 111..1 */
        return (x ^ mask) - mask;
}

ASL_int64_t
ASL_cvt_bits_uint_64(int width, ASL_bits64_t x)
{
        return x;
}

ASL_bits64_t
ASL_cvt_int_bits_64(int width, ASL_int64_t x)
{
        return x & ASL_mk_mask_64(width);
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
ASL_ones_bits_64(int width)
{
        return ASL_mk_mask_64(width);
}

ASL_bits64_t
ASL_or_bits_64(int width, ASL_bits64_t x, ASL_bits64_t y)
{
        return x | y;
}

ASL_bits64_t
ASL_sub_bits_64(int width, ASL_bits64_t x, ASL_bits64_t y)
{
        return (x - y) & ASL_mk_mask_64(width);
}

ASL_bits64_t
ASL_zeros_bits_64(int width)
{
        return 0;
}

////////////////////////////////////////////////////////////////
// End
////////////////////////////////////////////////////////////////
