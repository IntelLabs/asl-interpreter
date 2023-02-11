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

TEST_F(Bits64, Eor)
{
    EXPECT_EQ(zeros, ASL_eor_bits_64(64, zeros, zeros));
    EXPECT_EQ(zeros, ASL_eor_bits_64(64,  ones,  ones));
    EXPECT_EQ( ones, ASL_eor_bits_64(64, zeros,  ones));
}

TEST_F(Bits64, MkMask)
{
    EXPECT_EQ(1ULL, ASL_mk_mask_64(1));
}

TEST_F(Bits64, Not)
{
    int width = 63;
    ASL_bits64_t mask = ASL_mk_mask_64(width);

    EXPECT_EQ(zeros, ASL_not_bits_64(width, mask));
    EXPECT_EQ(mask, ASL_not_bits_64(width, zeros));
}

TEST_F(Bits64, Or)
{
    EXPECT_EQ(zeros, ASL_or_bits_64(64, zeros, zeros));
    EXPECT_EQ( ones, ASL_or_bits_64(64,  ones,  ones));
    EXPECT_EQ( ones, ASL_or_bits_64(64, zeros,  ones));
}

#define EXPECT_BITS_EQ(size, expected, actual) \
    for (int i = 0; i < (size / 64); ++i) {    \
        EXPECT_EQ(expected.v[i], actual.v[i]); \
    }

#define ASL_bits_zero(sizeof_x) \
        ASL_CC(ASL_bits_zero_, sizeof_x)()

#define ASL_bits_max(sizeof_x) \
        ASL_CC(ASL_bits_max_, sizeof_x)()

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
