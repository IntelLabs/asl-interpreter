////////////////////////////////////////////////////////////////
// Runtime bitvector support library for ASL's C backend
//
// Copyright Intel Inc (c) 2023
// SPDX-Licence-Identifier: BSD-3-Clause
////////////////////////////////////////////////////////////////

#ifndef ASL_BITS128_H
#define ASL_BITS128_H

#include <stdint.h>

#include "asl/bits64.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef struct {
        uint64_t v[2];
} ASL_bits128_t;

static inline ASL_bits128_t
ASL_bits_128(uint64_t v1, uint64_t v0)
{
        return (ASL_bits128_t){ { v0, v1 } };
}

static inline ASL_bits128_t
ASL_bits_zero_128()
{
        return ASL_bits_128(0, 0);
}

static inline ASL_bits128_t
ASL_bits_max_128()
{
        return ASL_bits_128(UINT64_MAX, UINT64_MAX);
}

static inline ASL_bits64_t
ASL_cast_bits_128_64(ASL_bits128_t x)
{
        return x.v[0];
}

ASL_bits128_t ASL_and_bits_128(ASL_bits128_t x, ASL_bits128_t y);
ASL_bits128_t ASL_lsr_bits_128(ASL_bits128_t x, int d);
ASL_bits128_t ASL_mask_128(int width);
ASL_bits128_t ASL_slice_lowd_128_128(ASL_bits128_t x, int lo, int width);
ASL_bits64_t ASL_slice_lowd_128_64(ASL_bits128_t x, int lo, int width);

#ifdef __cplusplus
}
#endif

#endif  // ASL_BITS128_H

////////////////////////////////////////////////////////////////
// End
////////////////////////////////////////////////////////////////
