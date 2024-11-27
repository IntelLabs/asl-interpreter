////////////////////////////////////////////////////////////////
// Runtime print support for ASL's C backend
//
// Copyright (C) 2023-2024 Intel Corporation
// SPDX-Licence-Identifier: BSD-3-Clause
////////////////////////////////////////////////////////////////

#ifndef ASL_PRINT_H
#define ASL_PRINT_H

#include <stdio.h>

#include "asl/bits64.h"
#include "asl/bits128.h"
#include "asl/bits256.h"
#include "asl/bits512.h"
#include "asl/bits1024.h"
#include "asl/integer.h"

#ifdef __cplusplus
extern "C" {
#endif

#define ASL_print_bits_hex(sizeof_x, n, x) \
        ASL_CC(ASL_print_bits_hex_, sizeof_x)(n, x)

static inline void
ASL_print_bits_hex_64(int width, ASL_bits64_t x)
{
        // as a special case, we don't zero pad small bitvectors
        printf("%d'x", width);
        printf("%llx", (long long)x);
}

#define N 128
#include "asl/print_template.h"
#undef N

#define N 256
#include "asl/print_template.h"
#undef N

#define N 512
#include "asl/print_template.h"
#undef N

#define N 1024
#include "asl/print_template.h"
#undef N

static inline void
ASL_print_char(ASL_int_t x)
{
        putchar(x);
}

static inline void
ASL_print_int_hex(ASL_int_t x)
{
#ifdef ASL_INT128
        int64_t top = (int64_t)(x >> 64);
        int64_t bottom = (int64_t)x;
        if (top == 0 && bottom >= 0) {
            printf("%#lx", bottom);
        } else if (top == -1 && bottom < 0 && bottom != INT64_MIN) {
            printf("-%#lx", -bottom);
        } else {
            // despite the name, large numbers are printed in hex
            printf("0x%08lx_%08lx", top, bottom);
        }
#else
        printf("0x%llx", (long long)x);
#endif
}

static inline void
ASL_print_int_dec(ASL_int_t x)
{
#ifdef ASL_INT128
        int64_t top = (int64_t)(x >> 64);
        int64_t bottom = (int64_t)x;
        if ((top == 0 && bottom >= 0) || (top == -1 && bottom < 0)) {
            printf("%ld", bottom);
        } else {
            // despite the name, large numbers are printed in hex
            printf("0x%08lx_%08lx", top, bottom);
        }
#else
        printf("%lld", (long long)x);
#endif
}

static inline void
ASL_print_str(const char* x)
{
        printf("%s", x);
}

#ifdef __cplusplus
}
#endif

#endif  // ASL_PRINT_H

////////////////////////////////////////////////////////////////
// End
////////////////////////////////////////////////////////////////
