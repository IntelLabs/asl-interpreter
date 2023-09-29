////////////////////////////////////////////////////////////////
// Runtime integer support library for ASL's C backend
//
// Copyright Intel Inc (c) 2023
// SPDX-Licence-Identifier: BSD-3-Clause
////////////////////////////////////////////////////////////////

#ifndef ASL_INTEGER_H
#define ASL_INTEGER_H

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef int64_t ASL_int64_t;

typedef struct {
        uint64_t u64[2];
} ASL_int128_t;

typedef ASL_int64_t ASL_int_t;

#define ASL_int_zero(sizeof_x) \
        ASL_CC(ASL_int_zero_, sizeof_x)()

static inline ASL_int64_t
ASL_int_zero_64()
{
        return 0;
}

#define ASL_int_max(sizeof_x) \
        ASL_CC(ASL_int_max_, sizeof_x)()

static inline ASL_int64_t
ASL_int_max_64()
{
        return INT64_MAX;
}

static inline ASL_int128_t
ASL_int_128(uint64_t v1, uint64_t v0)
{
        return (ASL_int128_t){ { v0, v1 } };
}

static inline ASL_int128_t
ASL_int_zero_128()
{
        return ASL_int_128(0, 0);
}

static inline ASL_int128_t
ASL_int_max_128()
{
        return ASL_int_128(INT64_MAX, UINT64_MAX);
}

#ifdef __cplusplus
}
#endif

#endif  // ASL_INTEGER_H

////////////////////////////////////////////////////////////////
// End
////////////////////////////////////////////////////////////////
