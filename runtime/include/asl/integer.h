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
        uint64_t v[2];
} ASL_int128_t;

typedef struct {
        uint64_t v[4];
} ASL_int256_t;

typedef struct {
        uint64_t v[8];
} ASL_int512_t;

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
        return ASL_int_128(UINT64_MAX, UINT64_MAX);
}

static inline ASL_int256_t
ASL_int_256(uint64_t v3, uint64_t v2, uint64_t v1, uint64_t v0)
{
        return (ASL_int256_t){ { v0, v1, v2, v3 } };
}

static inline ASL_int256_t
ASL_int_zero_256()
{
        return ASL_int_256(0, 0, 0, 0);
}

static inline ASL_int256_t
ASL_int_max_256()
{
        return ASL_int_256(UINT64_MAX, UINT64_MAX,
                           UINT64_MAX, UINT64_MAX);
}

static inline ASL_int512_t
ASL_int_512(uint64_t v7, uint64_t v6, uint64_t v5, uint64_t v4,
            uint64_t v3, uint64_t v2, uint64_t v1, uint64_t v0)
{
        return (ASL_int512_t){ { v0, v1, v2, v3, v4, v5, v6, v7 } };
}

static inline ASL_int512_t
ASL_int_zero_512()
{
        return ASL_int_512(0, 0, 0, 0, 0, 0, 0, 0);
}

static inline ASL_int512_t
ASL_int_max_512()
{
        return ASL_int_512(UINT64_MAX, UINT64_MAX,
                           UINT64_MAX, UINT64_MAX,
                           UINT64_MAX, UINT64_MAX,
                           UINT64_MAX, UINT64_MAX);
}

#ifdef __cplusplus
}
#endif

#endif  // ASL_INTEGER_H

////////////////////////////////////////////////////////////////
// End
////////////////////////////////////////////////////////////////
