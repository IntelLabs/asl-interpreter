////////////////////////////////////////////////////////////////
// Runtime bitvector support library for ASL's C backend
//
// Copyright Intel Inc (c) 2023
// SPDX-Licence-Identifier: BSD-3-Clause
////////////////////////////////////////////////////////////////

#include "asl/bits512.h"

#define N 512
#include "bits_template_c.h"
#undef N

ASL_bits512_t
ASL_and_bits_512(ASL_bits512_t x, ASL_bits512_t y)
{
        for (int i = 0; i < 8; ++i)
             x.v[i] &= y.v[i];
        return x;
}

ASL_bits512_t
ASL_lsr_bits_512(ASL_bits512_t x, int d)
{
        if (d == 0)
                return x;
        if (d < 64) {
                x.v[0] = (x.v[1] << (64 - d)) | (x.v[0] >> d);
                x.v[1] = (x.v[2] << (64 - d)) | (x.v[1] >> d);
                x.v[2] = (x.v[3] << (64 - d)) | (x.v[2] >> d);
                x.v[3] = (x.v[4] << (64 - d)) | (x.v[3] >> d);
                x.v[4] = (x.v[5] << (64 - d)) | (x.v[4] >> d);
                x.v[5] = (x.v[6] << (64 - d)) | (x.v[5] >> d);
                x.v[6] = (x.v[7] << (64 - d)) | (x.v[6] >> d);
                x.v[7] = x.v[7] >> d;
        } else {
                x.v[0] = x.v[1];
                x.v[1] = x.v[2];
                x.v[2] = x.v[3];
                x.v[3] = x.v[4];
                x.v[4] = x.v[5];
                x.v[5] = x.v[6];
                x.v[6] = x.v[7];
                x.v[7] = 0;
                x = ASL_lsr_bits_512(x, d - 64);
        }
        return x;
}

ASL_bits512_t
ASL_mk_mask_512(int width)
{
        return ASL_lsr_bits_512(ASL_bits_max_512(), 512 - width);
}

ASL_bits512_t
ASL_slice_lowd_512_512(ASL_bits512_t x, int lo, int width)
{
        x = ASL_lsr_bits_512(x, lo);
        return ASL_and_bits_512(x, ASL_mk_mask_512(width));
}

ASL_bits256_t
ASL_slice_lowd_512_256(ASL_bits512_t x, int lo, int width)
{
        return ASL_cast_bits_512_256(ASL_slice_lowd_512_512(x, lo, width));
}

ASL_bits128_t
ASL_slice_lowd_512_128(ASL_bits512_t x, int lo, int width)
{
        return ASL_cast_bits_512_128(ASL_slice_lowd_512_512(x, lo, width));
}

ASL_bits64_t
ASL_slice_lowd_512_64(ASL_bits512_t x, int lo, int width)
{
        return ASL_cast_bits_512_64(ASL_slice_lowd_512_512(x, lo, width));
}

////////////////////////////////////////////////////////////////
// End
////////////////////////////////////////////////////////////////
