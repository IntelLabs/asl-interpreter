////////////////////////////////////////////////////////////////
// Runtime bitvector support library for ASL's C backend
//
// Copyright Intel Inc (c) 2023
// SPDX-Licence-Identifier: BSD-3-Clause
////////////////////////////////////////////////////////////////

#include "asl/bits256.h"

#include <stdbool.h>

#define N 256
#include "bits_template_c.h"
#undef N

ASL_bits256_t
ASL_lsl_bits_256(int width, ASL_bits256_t x, int d)
{
        if (d == 0)
                return x;
        if (d < 64) {
                x.v[3] = (x.v[2] >> (64 - d)) | (x.v[3] << d);
                x.v[2] = (x.v[1] >> (64 - d)) | (x.v[2] << d);
                x.v[1] = (x.v[0] >> (64 - d)) | (x.v[1] << d);
                x.v[0] = x.v[0] << d;
        } else {
                x.v[3] = x.v[2];
                x.v[2] = x.v[1];
                x.v[1] = x.v[0];
                x.v[0] = 0;
                x = ASL_lsl_bits_256(width, x, d - 64);
        }
        return ASL_and_bits_256(width, x, ASL_mk_mask_256(width));
}

ASL_bits256_t
ASL_lsr_bits_256(ASL_bits256_t x, int d)
{
        if (d == 0)
                return x;
        if (d < 64) {
                x.v[0] = (x.v[1] << (64 - d)) | (x.v[0] >> d);
                x.v[1] = (x.v[2] << (64 - d)) | (x.v[1] >> d);
                x.v[2] = (x.v[3] << (64 - d)) | (x.v[2] >> d);
                x.v[3] = x.v[3] >> d;
        } else {
                x.v[0] = x.v[1];
                x.v[1] = x.v[2];
                x.v[2] = x.v[3];
                x.v[3] = 0;
                x = ASL_lsr_bits_256(x, d - 64);
        }
        return x;
}

ASL_bits256_t
ASL_mk_mask_256(int width)
{
        return ASL_lsr_bits_256(ASL_bits_max_256(), 256 - width);
}

ASL_bits256_t
ASL_slice_lowd_256_256(ASL_bits256_t x, int lo, int width)
{
        x = ASL_lsr_bits_256(x, lo);
        return ASL_and_bits_256(256, x, ASL_mk_mask_256(width));
}

ASL_bits128_t
ASL_slice_lowd_256_128(ASL_bits256_t x, int lo, int width)
{
        return ASL_cast_bits_256_128(ASL_slice_lowd_256_256(x, lo, width));
}

ASL_bits64_t
ASL_slice_lowd_256_64(ASL_bits256_t x, int lo, int width)
{
        return ASL_cast_bits_256_64(ASL_slice_lowd_256_256(x, lo, width));
}

////////////////////////////////////////////////////////////////
// End
////////////////////////////////////////////////////////////////
