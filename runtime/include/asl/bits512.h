////////////////////////////////////////////////////////////////
// Runtime bitvector support library for ASL's C backend
//
// Copyright Intel Inc (c) 2023
// SPDX-Licence-Identifier: BSD-3-Clause
////////////////////////////////////////////////////////////////

#ifndef ASL_BITS512_H
#define ASL_BITS512_H

#include <stdint.h>

#include "asl/bits64.h"
#include "asl/bits128.h"
#include "asl/bits256.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef struct {
        uint64_t v[8];
} ASL_bits512_t;

static inline ASL_bits512_t
ASL_bits_512(uint64_t v7, uint64_t v6, uint64_t v5, uint64_t v4, uint64_t v3,
             uint64_t v2, uint64_t v1, uint64_t v0)
{
        return (ASL_bits512_t){ { v0, v1, v2, v3, v4, v5, v6, v7 } };
}

static inline ASL_bits512_t
ASL_bits_zero_512()
{
        return ASL_bits_512(0, 0, 0, 0, 0, 0, 0, 0);
}

static inline ASL_bits512_t
ASL_bits_max_512()
{
        return ASL_bits_512(UINT64_MAX, UINT64_MAX, UINT64_MAX, UINT64_MAX,
                            UINT64_MAX, UINT64_MAX, UINT64_MAX, UINT64_MAX);
}

static inline ASL_bits64_t
ASL_cast_bits_512_64(ASL_bits512_t x)
{
        return x.v[0];
}

static inline ASL_bits128_t
ASL_cast_bits_512_128(ASL_bits512_t x)
{
        return ASL_bits_128(x.v[1], x.v[0]);
}

static inline ASL_bits256_t
ASL_cast_bits_512_256(ASL_bits512_t x)
{
        return ASL_bits_256(x.v[3], x.v[2], x.v[1], x.v[0]);
}

ASL_bits512_t ASL_and_bits_512(ASL_bits512_t x, ASL_bits512_t y);
ASL_bits512_t ASL_lsr_bits_512(ASL_bits512_t x, int d);
ASL_bits512_t ASL_mk_mask_512(int width);
ASL_bits512_t ASL_slice_lowd_512_512(ASL_bits512_t x, int lo, int width);
ASL_bits256_t ASL_slice_lowd_512_256(ASL_bits512_t x, int lo, int width);
ASL_bits128_t ASL_slice_lowd_512_128(ASL_bits512_t x, int lo, int width);
ASL_bits64_t ASL_slice_lowd_512_64(ASL_bits512_t x, int lo, int width);

#ifdef __cplusplus
}
#endif

#endif  // ASL_BITS512_H

////////////////////////////////////////////////////////////////
// End
////////////////////////////////////////////////////////////////
