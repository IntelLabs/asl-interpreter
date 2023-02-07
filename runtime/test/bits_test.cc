#include "asl/bits64.h"
#include "asl/bits128.h"
#include "asl/bits256.h"
#include "asl/bits512.h"

#include "gtest/gtest.h"

class Bits64 : public ::testing::Test {
 protected:
    ASL_bits64_t zeros = 0;
};

TEST_F(Bits64, Not)
{
    int width = 63;
    ASL_bits64_t mask = ASL_mk_mask_64(width);

    EXPECT_EQ(zeros, ASL_not_bits_64(width, mask));
    EXPECT_EQ(mask, ASL_not_bits_64(width, zeros));
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
