////////////////////////////////////////////////////////////////
// Runtime bitvector support library for ASL's C backend
//
// Copyright Intel Inc (c) 2022
// SPDX-Licence-Identifier: BSD-3-Clause
////////////////////////////////////////////////////////////////

#ifndef ASL_BITS64_H
#define ASL_BITS64_H

#include <stdbool.h>
#include <stdint.h>

#include "asl/integer.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef uint64_t ASL_bits64_t;

#define ASL_CC(x, y) x##y
#define ASL_CC_INDIR(x, y) ASL_CC(x, y)

#define ASL_bits_max(sizeof_x) \
        ASL_CC(ASL_bits_max_, sizeof_x)()

static inline ASL_bits64_t
ASL_bits_max_64()
{
        return UINT64_MAX;
}

#define ASL_and_bits(sizeof_x, n, x, y) \
        ASL_CC(ASL_and_bits_, sizeof_x)(n, x, y)

ASL_bits64_t ASL_and_bits_64(int width, ASL_bits64_t x, ASL_bits64_t y);

static inline int64_t
ASL_cvt_bits_sint(ASL_bits64_t x, int x_width)
{
        const uint64_t mask = 1ULL << (x_width - 1);
        /* If the sign bit is 1 then, after XOR-ing,
           the subtraction borrows from higher bits making them 111..1 */
        return (x ^ mask) - mask;
}

#define ASL_cvt_int_bits(sizeof_x, n, x) \
        ASL_CC(ASL_cvt_int_bits_, sizeof_x)(n, x)

ASL_bits64_t ASL_cvt_int_bits_64(int width, ASL_int64_t x);

#define ASL_eor_bits(sizeof_x, n, x, y) \
        ASL_CC(ASL_eor_bits_, sizeof_x)(n, x, y)

ASL_bits64_t ASL_eor_bits_64(int width, ASL_bits64_t x, ASL_bits64_t y);

#define ASL_eq_bits(sizeof_x, n, x, y) \
        ASL_CC(ASL_eq_bits_, sizeof_x)(n, x, y)

bool ASL_eq_bits_64(int width, ASL_bits64_t x, ASL_bits64_t y);

#define ASL_lsl_bits(sizeof_x, n, x, d) \
        ASL_CC(ASL_lsl_bits_, sizeof_x)(n, x, d)

ASL_bits64_t ASL_lsl_bits_64(int width, ASL_bits64_t x, int d);

#define ASL_lsr_bits(sizeof_x, n, x, d) \
        ASL_CC(ASL_lsr_bits_, sizeof_x)(n, x, d)

ASL_bits64_t ASL_lsr_bits_64(int width, ASL_bits64_t x, int d);

static inline ASL_bits64_t
ASL_asr_bits(ASL_bits64_t x, int d)
{
        /* Note: it is implementation-defined whether this performs
         * an arithmetic shift or a logical shift.
         * On gcc, it performs an arithmetic shift.
         */
        return (ASL_bits64_t)(((int64_t) x) >> d);
}

static inline ASL_bits64_t
ASL_replicate_bits(ASL_bits64_t x, int n, int x_width)
{
        ASL_bits64_t r = 0;
        while (n-- > 0)
                r = (r << x_width) | x;
        return r;
}

#define ASL_mk_mask(n, w) \
        ASL_CC(ASL_mk_mask_, n)(w)

ASL_bits64_t ASL_mk_mask_64(int w);

#define ASL_ne_bits(sizeof_x, n, x, y) \
        ASL_CC(ASL_ne_bits_, sizeof_x)(n, x, y)

bool ASL_ne_bits_64(int width, ASL_bits64_t x, ASL_bits64_t y);

#define ASL_not_bits(sizeof_x, n, x) \
        ASL_CC(ASL_not_bits_, sizeof_x)(n, x)

ASL_bits64_t ASL_not_bits_64(int width, ASL_bits64_t x);

#define ASL_or_bits(sizeof_x, n, x, y) \
        ASL_CC(ASL_or_bits_, sizeof_x)(n, x, y)

ASL_bits64_t ASL_or_bits_64(int width, ASL_bits64_t x, ASL_bits64_t y);

#define ASL_slice_lowd(sizeof_x, sizeof_res, x, lo, width) \
        ASL_slice_lowd_##sizeof_x##_##sizeof_res((x), (lo), (width))

static inline ASL_bits64_t
ASL_slice_lowd_64_64(ASL_bits64_t x, int lo, int width)
{
        return (x >> lo) & ASL_mk_mask_64(width);
}

static inline ASL_bits64_t
ASL_slice_hilo(ASL_bits64_t x, int hi, int lo)
{
        return (x & ASL_mk_mask_64(hi + 1)) >> lo;
}

static inline ASL_bits64_t
ASL_slice_lowd_w(ASL_bits64_t val, ASL_bits64_t x, int lo, int width)
{
        return (x & ~(ASL_mk_mask_64(width) << lo)) | (val << lo);
}

static inline ASL_bits64_t
ASL_slice_hilo_w(ASL_bits64_t val, ASL_bits64_t x, int hi, int lo)
{
        return (x & ~(ASL_mk_mask_64(hi - lo + 1) << lo)) | (val << lo);
}

#ifdef __cplusplus
}
#endif

#endif  // ASL_BITS64_H

////////////////////////////////////////////////////////////////
// End
////////////////////////////////////////////////////////////////
