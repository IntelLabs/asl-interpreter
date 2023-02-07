#define ASL_BITS_TYPE ASL_CC_INDIR(ASL_CC_INDIR(ASL_bits, N), _t)

class ASL_CC_INDIR(Bits, N) : public ::testing::Test {
 protected:
    ASL_BITS_TYPE zeros = ASL_bits_zero(N);
};

TEST_F(ASL_CC_INDIR(Bits, N), Not)
{
    int width = N - 1;
    ASL_BITS_TYPE mask = ASL_mk_mask(N, width);

    EXPECT_BITS_EQ(N, zeros, ASL_not_bits(N, width, mask));
    EXPECT_BITS_EQ(N, mask, ASL_not_bits(N, width, zeros));
}

#undef ASL_BITS_TYPE
