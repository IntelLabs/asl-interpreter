#define ASL_BITS_CHUNKS N >> 6
#define ASL_BITS_TYPE ASL_CC_INDIR(ASL_CC_INDIR(ASL_bits, N), _t)
#define ASL_INT_TYPE ASL_CC_INDIR(ASL_CC_INDIR(ASL_int, N), _t)

class ASL_CC_INDIR(Bits, N) : public ::testing::Test {
 protected:
    ASL_BITS_TYPE zeros = ASL_zeros_bits(N, N);
    ASL_BITS_TYPE ones = ASL_bits_max(N);

    ASL_INT_TYPE minus_one = ASL_int_max(N);
    ASL_INT_TYPE zero = ASL_int_zero(N);
};

TEST_F(ASL_CC_INDIR(Bits, N), Add)
{
    int width = N - 1;
    ASL_BITS_TYPE one = ASL_mk_mask(N, 1);
    ASL_BITS_TYPE minus_one = ASL_mk_mask(N, width);
    ASL_BITS_TYPE two = ASL_lsl_bits(N, width, one, 1);
    ASL_BITS_TYPE minus_two = ASL_not_bits(N, width, one);

    EXPECT_BITS_EQ(N, two, ASL_add_bits(N, width, one, one));
    EXPECT_BITS_EQ(N, minus_two, ASL_add_bits(N, width, minus_one, minus_one));
    EXPECT_BITS_EQ(N, zeros, ASL_add_bits(N, width, minus_one, one));
}

TEST_F(ASL_CC_INDIR(Bits, N), And)
{
    EXPECT_BITS_EQ(N, zeros, ASL_and_bits(N, N, zeros, zeros));
    EXPECT_BITS_EQ(N,  ones, ASL_and_bits(N, N,  ones,  ones));
    EXPECT_BITS_EQ(N, zeros, ASL_and_bits(N, N, zeros,  ones));
}

TEST_F(ASL_CC_INDIR(Bits, N), Asr)
{
    int shift = 1;
    int width = N - 1;
    ASL_BITS_TYPE x = ASL_mk_mask(N, width);

    EXPECT_BITS_EQ(N, zeros, ASL_asr_bits(N, width, zeros, shift));
    EXPECT_BITS_EQ(N, x, ASL_asr_bits(N, width, x, shift));
}

TEST_F(ASL_CC_INDIR(Bits, N), CvtBitsSInt)
{
    int width = N - 1;
    ASL_BITS_TYPE x = ASL_mk_mask(N, width);

    EXPECT_BITS_EQ(N, minus_one, ASL_cvt_bits_sint(N, width, x));
    EXPECT_BITS_EQ(N, zero, ASL_cvt_bits_sint(N, width, zeros));
}

TEST_F(ASL_CC_INDIR(Bits, N), CvtBitsUInt)
{
    int width = N - 1;
    ASL_BITS_TYPE x = ASL_mk_mask(N, width);
    ASL_INT_TYPE r;
    for (int i = 0; i < ASL_BITS_CHUNKS; ++i)
         r.v[i] = x.v[i];

    EXPECT_BITS_EQ(N, r, ASL_cvt_bits_uint(N, width, x));
}

TEST_F(ASL_CC_INDIR(Bits, N), CvtIntBits)
{
    int width = N - 1;
    ASL_BITS_TYPE r = ASL_mk_mask(N, width);

    EXPECT_BITS_EQ(N, r, ASL_cvt_int_bits(N, width, minus_one));
}

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

TEST_F(ASL_CC_INDIR(Bits, N), Ones)
{
    int width = N - 1;
    ASL_BITS_TYPE mask = ASL_mk_mask(N, width);

    EXPECT_BITS_EQ(N, mask, ASL_ones_bits(N, width));
}

TEST_F(ASL_CC_INDIR(Bits, N), Or)
{
    EXPECT_BITS_EQ(N, zeros, ASL_or_bits(N, N, zeros, zeros));
    EXPECT_BITS_EQ(N,  ones, ASL_or_bits(N, N,  ones,  ones));
    EXPECT_BITS_EQ(N,  ones, ASL_or_bits(N, N, zeros,  ones));
}

TEST_F(ASL_CC_INDIR(Bits, N), Sub)
{
    int width = N - 1;
    ASL_BITS_TYPE one = ASL_mk_mask(N, 1);
    ASL_BITS_TYPE minus_one = ASL_mk_mask(N, width);

    EXPECT_BITS_EQ(N, zeros, ASL_sub_bits(N, width, one, one));
    EXPECT_BITS_EQ(N, minus_one, ASL_sub_bits(N, width, zeros, one));
    EXPECT_BITS_EQ(N, one, ASL_sub_bits(N, width, zeros, minus_one));
}

#undef ASL_BITS_CHUNKS
#undef ASL_BITS_TYPE
#undef ASL_INT_TYPE
