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
ASL_and_bits_128(ASL_bits128_t x, ASL_bits128_t y)
{
        for (int i = 0; i < 2; ++i)
             x.v[i] &= y.v[i];
        return x;
}

ASL_bits128_t
ASL_lsr_bits_128(ASL_bits128_t x, int d)
{
        if (d == 0)
                return x;
        if (d < 64) {
                x.v[0] = (x.v[1] << (64 - d)) | (x.v[0] >> d);
                x.v[1] = x.v[1] >> d;
        } else {
                x.v[0] = x.v[1];
                x.v[1] = 0;
                x = ASL_lsr_bits_128(x, d - 64);
        }
        return x;
}

ASL_bits128_t
ASL_mk_mask_128(int width)
{
        return ASL_lsr_bits_128(ASL_bits_max_128(), 128 - width);
}

ASL_bits128_t
ASL_slice_lowd_128_128(ASL_bits128_t x, int lo, int width)
{
        x = ASL_lsr_bits_128(x, lo);
        return ASL_and_bits_128(x, ASL_mk_mask_128(width));
}

ASL_bits64_t
ASL_slice_lowd_128_64(ASL_bits128_t x, int lo, int width)
{
        return ASL_cast_bits_128_64(ASL_slice_lowd_128_128(x, lo, width));
}

////////////////////////////////////////////////////////////////
// End
////////////////////////////////////////////////////////////////
