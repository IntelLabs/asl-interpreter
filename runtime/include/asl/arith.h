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

static inline int64_t
ASL_cvt_bits_sint(uint64_t x, int x_width)
{
        const uint64_t mask = 1ULL << (x_width - 1);
        /* If the sign bit is 1 then, after XOR-ing,
           the subtraction borrows from higher bits making them 111..1 */
        return (x ^ mask) - mask;
}

static inline bool
ASL_is_pow2_int(uint64_t x)
{
        return x != 0 && (x & (x - 1)) == 0;
}

static inline uint64_t
ASL_lsl_bits(uint64_t x, int d)
{
        return x << d;
}

static inline uint64_t
ASL_lsr_bits(uint64_t x, int d)
{
        return x >> d;
}

static inline uint64_t
ASL_asr_bits(uint64_t x, int d)
{
        /* Note: it is implementation-defined whether this performs
         * an arithmetic shift or a logical shift.
         * On gcc, it performs an arithmetic shift.
         */
        return (uint64_t)(((int64_t) x) >> d);
}

static inline uint64_t
ASL_replicate_bits(uint64_t x, int n, int x_width)
{
        uint64_t r = 0;
        while (n-- > 0)
                r = (r << x_width) | x;
        return r;
}

#define ASL_mask(width) ((1ULL << (width)) - 1)

static inline uint64_t
ASL_mk_mask(int w, int n)
{
    return ASL_mask(w);
}

static inline uint64_t
ASL_slice_lowd(uint64_t x, int lo, int width)
{
        return (x >> lo) & ASL_mask(width);
}

static inline uint64_t
ASL_slice_hilo(uint64_t x, int hi, int lo)
{
        return (x & ASL_mask(hi + 1)) >> lo;
}

static inline uint64_t
ASL_slice_lowd_w(uint64_t val, uint64_t x, int lo, int width)
{
        return (x & ~(ASL_mask(width) << lo)) | (val << lo);
}

static inline uint64_t
ASL_slice_hilo_w(uint64_t val, uint64_t x, int hi, int lo)
{
        return (x & ~(ASL_mask(hi - lo + 1) << lo)) | (val << lo);
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
