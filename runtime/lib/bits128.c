////////////////////////////////////////////////////////////////
// Runtime bitvector support library for ASL's C backend
//
// Copyright Intel Inc (c) 2023
// SPDX-Licence-Identifier: BSD-3-Clause
////////////////////////////////////////////////////////////////

#include "asl/bits128.h"

#include <stdbool.h>

#define N 128
#include "bits_template_c.h"
#undef N

ASL_bits128_t
ASL_lsl_bits_128(int width, ASL_bits128_t x, int d)
{
        if (d == 0)
                return x;
        if (d < 64) {
                x.u64[1] = (x.u64[0] >> (64 - d)) | (x.u64[1] << d);
                x.u64[0] = x.u64[0] << d;
        } else {
                x.u64[1] = x.u64[0];
                x.u64[0] = 0;
                x = ASL_lsl_bits_128(width, x, d - 64);
        }
        return ASL_and_bits_128(width, x, ASL_mk_mask_128(width));
}

ASL_bits128_t
ASL_lsr_bits_128(int width, ASL_bits128_t x, int d)
{
        if (d == 0)
                return x;
        if (d < 64) {
                x.u64[0] = (x.u64[1] << (64 - d)) | (x.u64[0] >> d);
                x.u64[1] = x.u64[1] >> d;
        } else {
                x.u64[0] = x.u64[1];
                x.u64[1] = 0;
                x = ASL_lsr_bits_128(width, x, d - 64);
        }
        return x;
}

ASL_bits128_t
ASL_replicate_bits_64_128(int width, ASL_bits64_t x, int n)
{
        return ASL_replicate_bits_128_128(width, ASL_cast_bits_64_128(x), n);
}

ASL_bits64_t
ASL_slice_lowd_128_64(ASL_bits128_t x, int lo, int width)
{
        return ASL_cast_bits_128_64(ASL_slice_lowd_128_128(x, lo, width));
}

ASL_bits128_t
ASL_zero_extend_bits_64_128(int width, ASL_bits64_t x, int n)
{
        return ASL_bits_128(0, x);
}

ASL_bits128_t
ASL_zeros_bits_128(int width)
{
        return ASL_bits_128(0, 0);
}

////////////////////////////////////////////////////////////////
// End
////////////////////////////////////////////////////////////////
