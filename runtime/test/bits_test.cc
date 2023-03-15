////////////////////////////////////////////////////////////////
// Tests for C runtime bitvector support library
//
// Copyright Intel Inc (c) 2023
// SPDX-Licence-Identifier: BSD-3-Clause
////////////////////////////////////////////////////////////////

#include "asl/bits64.h"
#include "asl/bits128.h"
#include "asl/bits256.h"
#include "asl/bits512.h"

#include "gtest/gtest.h"

class Bits64 : public ::testing::Test {
 protected:
    ASL_bits64_t zeros = 0;
    ASL_bits64_t ones = UINT64_MAX;
};

TEST_F(Bits64, And)
{
    EXPECT_EQ(zeros, ASL_and_bits_64(64, zeros, zeros));
    EXPECT_EQ( ones, ASL_and_bits_64(64,  ones,  ones));
    EXPECT_EQ(zeros, ASL_and_bits_64(64, zeros,  ones));
}

TEST_F(Bits64, CvtBitsUInt)
{
    int width = 3;

    EXPECT_EQ(2LL, ASL_cvt_bits_uint_64(width, 2ULL));
    EXPECT_EQ(4LL, ASL_cvt_bits_uint_64(width, 4ULL));
}

TEST_F(Bits64, CvtIntBits)
{
    int width = 3;

    EXPECT_EQ(6ULL, ASL_cvt_int_bits_64(width, 14LL));
    EXPECT_EQ(6ULL, ASL_cvt_int_bits_64(width, -2LL));
}

TEST_F(Bits64, Eor)
{
    EXPECT_EQ(zeros, ASL_eor_bits_64(64, zeros, zeros));
    EXPECT_EQ(zeros, ASL_eor_bits_64(64,  ones,  ones));
    EXPECT_EQ( ones, ASL_eor_bits_64(64, zeros,  ones));
}

TEST_F(Bits64, Eq)
{
    EXPECT_FALSE(ASL_eq_bits_64(64, zeros, ones));
    EXPECT_TRUE(ASL_eq_bits_64(64, ones, ones));
}

TEST_F(Bits64, Lsl)
{
    int shift = 1;
    int width = 2;

    EXPECT_EQ(2ULL, ASL_lsl_bits_64(width, 3ULL, shift));
}

TEST_F(Bits64, Lsr)
{
    int shift = 1;
    int width = 64;

    EXPECT_EQ(1ULL, ASL_lsr_bits_64(width, 2ULL, shift));
}

TEST_F(Bits64, MkMask)
{
    EXPECT_EQ(1ULL, ASL_mk_mask_64(1));
}

TEST_F(Bits64, Ne)
{
    EXPECT_TRUE(ASL_ne_bits_64(64, zeros, ones));
    EXPECT_FALSE(ASL_ne_bits_64(64, ones, ones));
}

TEST_F(Bits64, Not)
{
    int width = 63;
    ASL_bits64_t mask = ASL_mk_mask_64(width);

    EXPECT_EQ(zeros, ASL_not_bits_64(width, mask));
    EXPECT_EQ(mask, ASL_not_bits_64(width, zeros));
}

TEST_F(Bits64, Ones)
{
    EXPECT_EQ(7ULL, ASL_ones_bits_64(3));
}

TEST_F(Bits64, Or)
{
    EXPECT_EQ(zeros, ASL_or_bits_64(64, zeros, zeros));
    EXPECT_EQ( ones, ASL_or_bits_64(64,  ones,  ones));
    EXPECT_EQ( ones, ASL_or_bits_64(64, zeros,  ones));
}

TEST_F(Bits64, Zeros)
{
    EXPECT_EQ(0ULL, ASL_zeros_bits_64(3));
}

#define EXPECT_BITS_EQ(size, expected, actual) \
    for (int i = 0; i < (size / 64); ++i) {    \
        EXPECT_EQ(expected.v[i], actual.v[i]); \
    }

#define N 128
#include "bits_test_template.h"
#undef N

#define N 256
#include "bits_test_template.h"
#undef N

#define N 512
#include "bits_test_template.h"
#undef N

#define EXPECT_BITS128_EQ(expected, actual) \
    EXPECT_BITS_EQ(128, expected, actual)

#define EXPECT_BITS256_EQ(expected, actual) \
    EXPECT_BITS_EQ(256, expected, actual)

#define EXPECT_BITS512_EQ(expected, actual) \
    EXPECT_BITS_EQ(512, expected, actual)

TEST_F(Bits128, Lsl)
{
    ASL_bits128_t x = ASL_bits_128(3, 3);
    int shift = 1;
    int width = 64 + 2;

    EXPECT_BITS128_EQ(ASL_bits_128(2, 6),
                      ASL_lsl_bits_128(width, x, shift));
}

TEST_F(Bits256, Lsl)
{
    ASL_bits256_t x = ASL_bits_256(3, 3, 3, 3);
    int shift = 1;
    int width = 64 * 3 + 2;

    EXPECT_BITS256_EQ(ASL_bits_256(2, 6, 6, 6),
                      ASL_lsl_bits_256(width, x, shift));
}

TEST_F(Bits512, Lsl)
{
    ASL_bits512_t x = ASL_bits_512(3, 3, 3, 3, 3, 3, 3, 3);
    int shift = 1;
    int width = 64 * 7 + 2;

    EXPECT_BITS512_EQ(ASL_bits_512(2, 6, 6, 6, 6, 6, 6, 6),
                      ASL_lsl_bits_512(width, x, shift));
}

TEST_F(Bits128, LslBy64)
{
    ASL_bits128_t x = ASL_bits_128(3, 3);
    int shift = 64;
    int width = 64 + 1;

    EXPECT_BITS128_EQ(ASL_bits_128(1, 0),
                      ASL_lsl_bits_128(width, x, shift));
}

TEST_F(Bits256, LslBy64)
{
    ASL_bits256_t x = ASL_bits_256(3, 3, 3, 3);
    int shift = 64;
    int width = 64 * 3 + 1;

    EXPECT_BITS256_EQ(ASL_bits_256(1, 3, 3, 0),
                      ASL_lsl_bits_256(width, x, shift));
}

TEST_F(Bits512, LslBy64)
{
    ASL_bits512_t x = ASL_bits_512(3, 3, 3, 3, 3, 3, 3, 3);
    int shift = 64;
    int width = 64 * 7 + 1;

    EXPECT_BITS512_EQ(ASL_bits_512(1, 3, 3, 3, 3, 3, 3, 0),
                      ASL_lsl_bits_512(width, x, shift));
}

TEST_F(Bits128, LslGt64)
{
    ASL_bits128_t x = ASL_bits_128(3, 3);
    int shift = 64 + 1;
    int width = 64 + 2;

    EXPECT_BITS128_EQ(ASL_bits_128(2, 0),
                      ASL_lsl_bits_128(width, x, shift));
}

TEST_F(Bits256, LslGt64)
{
    ASL_bits256_t x = ASL_bits_256(3, 3, 3, 3);
    int shift = 64 + 1;
    int width = 64 * 3 + 2;

    EXPECT_BITS256_EQ(ASL_bits_256(2, 6, 6, 0),
                      ASL_lsl_bits_256(width, x, shift));
}

TEST_F(Bits512, LslGt64)
{
    ASL_bits512_t x = ASL_bits_512(3, 3, 3, 3, 3, 3, 3, 3);
    int shift = 64 + 1;
    int width = 64 * 7 + 2;

    EXPECT_BITS512_EQ(ASL_bits_512(2, 6, 6, 6, 6, 6, 6, 0),
                      ASL_lsl_bits_512(width, x, shift));
}

TEST_F(Bits128, Lsr)
{
    ASL_bits128_t x = ASL_bits_128(2, 2);
    int shift = 1;
    int width = 128;

    EXPECT_BITS128_EQ(ASL_bits_128(1, 1),
                      ASL_lsr_bits_128(width, x, shift));
}

TEST_F(Bits256, Lsr)
{
    ASL_bits256_t x = ASL_bits_256(2, 2, 2, 2);
    int shift = 1;
    int width = 256;

    EXPECT_BITS256_EQ(ASL_bits_256(1, 1, 1, 1),
                      ASL_lsr_bits_256(width, x, shift));
}

TEST_F(Bits512, Lsr)
{
    ASL_bits512_t x = ASL_bits_512(2, 2, 2, 2, 2, 2, 2, 2);
    int shift = 1;
    int width = 512;

    EXPECT_BITS512_EQ(ASL_bits_512(1, 1, 1, 1, 1, 1, 1, 1),
                      ASL_lsr_bits_512(width, x, shift));
}

TEST_F(Bits128, LsrBy64)
{
    ASL_bits128_t x = ASL_bits_128(1, 1);
    int shift = 64;
    int width = 128;

    EXPECT_BITS128_EQ(ASL_bits_128(0, 1),
                      ASL_lsr_bits_128(width, x, shift));
}

TEST_F(Bits256, LsrBy64)
{
    ASL_bits256_t x = ASL_bits_256(1, 1, 1, 1);
    int shift = 64;
    int width = 256;

    EXPECT_BITS256_EQ(ASL_bits_256(0, 1, 1, 1),
                      ASL_lsr_bits_256(width, x, shift));
}

TEST_F(Bits512, LsrBy64)
{
    ASL_bits512_t x = ASL_bits_512(1, 1, 1, 1, 1, 1, 1, 1);
    int shift = 64;
    int width = 512;

    EXPECT_BITS512_EQ(ASL_bits_512(0, 1, 1, 1, 1, 1, 1, 1),
                      ASL_lsr_bits_512(width, x, shift));
}

TEST_F(Bits128, LsrGt64)
{
    ASL_bits128_t x = ASL_bits_128(2, 2);
    int shift = 64 + 1;
    int width = 128;

    EXPECT_BITS128_EQ(ASL_bits_128(0, 1),
                      ASL_lsr_bits_128(width, x, shift));
}

TEST_F(Bits256, LsrGt64)
{
    ASL_bits256_t x = ASL_bits_256(2, 2, 2, 2);
    int shift = 64 + 1;
    int width = 256;

    EXPECT_BITS256_EQ(ASL_bits_256(0, 1, 1, 1),
                      ASL_lsr_bits_256(width, x, shift));
}

TEST_F(Bits512, LsrGt64)
{
    ASL_bits512_t x = ASL_bits_512(2, 2, 2, 2, 2, 2, 2, 2);
    int shift = 64 + 1;
    int width = 512;

    EXPECT_BITS512_EQ(ASL_bits_512(0, 1, 1, 1, 1, 1, 1, 1),
                      ASL_lsr_bits_512(width, x, shift));
}

TEST_F(Bits128, MkMask)
{
    EXPECT_BITS128_EQ(ASL_bits_128(1, UINT64_MAX),
                      ASL_mk_mask_128(1 + 64));
}

TEST_F(Bits256, MkMask)
{
    EXPECT_BITS256_EQ(ASL_bits_256(0, 1, UINT64_MAX, UINT64_MAX),
                      ASL_mk_mask_256(1 + 128));
}

TEST_F(Bits512, MkMask)
{
    EXPECT_BITS512_EQ(ASL_bits_512(0, 0, 0, 1, UINT64_MAX, UINT64_MAX, UINT64_MAX, UINT64_MAX),
                      ASL_mk_mask_512(1 + 256));
}

TEST_F(Bits128, Zeros)
{
    EXPECT_BITS128_EQ(ASL_bits_128(0, 0),
                      ASL_zeros_bits_128(1 + 64));
}

TEST_F(Bits256, Zeros)
{
    EXPECT_BITS256_EQ(ASL_bits_256(0, 0, 0, 0),
                      ASL_zeros_bits_256(1 + 128));
}

TEST_F(Bits512, Zeros)
{
    EXPECT_BITS512_EQ(ASL_bits_512(0, 0, 0, 0, 0, 0, 0, 0),
                      ASL_zeros_bits_512(1 + 256));
}
