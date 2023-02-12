#define ASL_BITS_TYPE ASL_CC_INDIR(ASL_CC_INDIR(ASL_bits, N), _t)

class ASL_CC_INDIR(Bits, N) : public ::testing::Test {
 protected:
    ASL_BITS_TYPE zeros = ASL_bits_zero(N);
    ASL_BITS_TYPE ones = ASL_bits_max(N);
};

TEST_F(ASL_CC_INDIR(Bits, N), Eor)
{
    EXPECT_BITS_EQ(N, zeros, ASL_eor_bits(N, N, zeros, zeros));
    EXPECT_BITS_EQ(N, zeros, ASL_eor_bits(N, N,  ones,  ones));
    EXPECT_BITS_EQ(N,  ones, ASL_eor_bits(N, N, zeros,  ones));
}

TEST_F(ASL_CC_INDIR(Bits, N), Eq)
{
    EXPECT_FALSE(ASL_eq_bits(N, N, zeros, ones));
    EXPECT_TRUE(ASL_eq_bits(N, N, ones, ones));
}

TEST_F(ASL_CC_INDIR(Bits, N), Ne)
{
    EXPECT_TRUE(ASL_ne_bits(N, N, zeros, ones));
    EXPECT_FALSE(ASL_ne_bits(N, N, ones, ones));
}

TEST_F(ASL_CC_INDIR(Bits, N), Not)
{
    int width = N - 1;
    ASL_BITS_TYPE mask = ASL_mk_mask(N, width);

    EXPECT_BITS_EQ(N, zeros, ASL_not_bits(N, width, mask));
    EXPECT_BITS_EQ(N, mask, ASL_not_bits(N, width, zeros));
}

TEST_F(ASL_CC_INDIR(Bits, N), Or)
{
    EXPECT_BITS_EQ(N, zeros, ASL_or_bits(N, N, zeros, zeros));
    EXPECT_BITS_EQ(N,  ones, ASL_or_bits(N, N,  ones,  ones));
    EXPECT_BITS_EQ(N,  ones, ASL_or_bits(N, N, zeros,  ones));
}

#undef ASL_BITS_TYPE
