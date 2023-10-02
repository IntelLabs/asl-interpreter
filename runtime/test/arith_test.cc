////////////////////////////////////////////////////////////////
// Tests for C runtime integer support library
//
// Copyright Intel Inc (c) 2023
// SPDX-Licence-Identifier: BSD-3-Clause
////////////////////////////////////////////////////////////////

#include "asl/arith.h"

#include "gtest/gtest.h"

class Arith : public ::testing::Test {
 protected:
};

TEST_F(Arith, MaskInt)
{
    EXPECT_EQ(1LL, ASL_mask_int(1LL));
    EXPECT_EQ(-1LL, ASL_mask_int(64LL));
}
