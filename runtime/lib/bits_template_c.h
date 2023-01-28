#define ASL_BITS_LIMBS_64 (N >> 6)
#define ASL_BITS_LIMBS_32 (N >> 5)
#define ASL_BITS_TYPE ASL_CC_INDIR(ASL_CC_INDIR(ASL_bits, N), _t)
#define ASL_INT_TYPE ASL_CC_INDIR(ASL_CC_INDIR(ASL_int, N), _t)

ASL_BITS_TYPE
ASL_add_bits(N, int width, ASL_BITS_TYPE x, ASL_BITS_TYPE y)
{
        ASL_BITS_TYPE r;
        uint64_t carry = 0;
        for (int i = 0; i < ASL_BITS_LIMBS_64; ++i) {
                 r.u64[i] = x.u64[i] + y.u64[i] + carry;
                 carry = (carry & (r.u64[i] == x.u64[i])) | (r.u64[i] < x.u64[i]);
        }
        return ASL_and_bits(N, width, r, ASL_mk_mask(N, width));
}

ASL_BITS_TYPE
ASL_and_bits(N, int width, ASL_BITS_TYPE x, ASL_BITS_TYPE y)
{
        for (int i = 0; i < ASL_BITS_LIMBS_64; ++i)
             x.u64[i] &= y.u64[i];
        return x;
}

ASL_BITS_TYPE
ASL_asr_bits(N, int width, ASL_BITS_TYPE x, int d)
{
        bool sign_bit = ASL_lsr_bits(N, width, x, width - 1).u64[0];
        if (sign_bit) {
                x = ASL_not_bits(N, width, x);
                x = ASL_lsr_bits(N, width, x, d);
                x = ASL_not_bits(N, width, x);
        } else {
                x = ASL_lsr_bits(N, width, x, d);
        }
        return x;
}

ASL_INT_TYPE
ASL_cvt_bits_sint(N, int width, ASL_BITS_TYPE x)
{
        bool sign_bit = ASL_lsr_bits(N, width, x, width - 1).u64[0];
        if (sign_bit) {
                x = ASL_or_bits(N, N, x,
                                ASL_not_bits(N, N, ASL_mk_mask(N, width)));
        }

        ASL_INT_TYPE r;
        for (int i = 0; i < ASL_BITS_LIMBS_64; ++i)
                r.u64[i] = x.u64[i];
        return r;
}

ASL_INT_TYPE
ASL_cvt_bits_uint(N, int width, ASL_BITS_TYPE x)
{
        ASL_INT_TYPE r;
        for (int i = 0; i < ASL_BITS_LIMBS_64; ++i)
             r.u64[i] = x.u64[i];
        return r;
}

ASL_BITS_TYPE
ASL_cvt_int_bits(N, int width, ASL_INT_TYPE x)
{
        ASL_BITS_TYPE r;
        for (int i = 0; i < ASL_BITS_LIMBS_64; ++i)
             r.u64[i] = x.u64[i];
        return ASL_and_bits(N, width, r, ASL_mk_mask(N, width));
}

bool
ASL_eq_bits(N, int width, ASL_BITS_TYPE x, ASL_BITS_TYPE y)
{
        for (int i = 0; i < ASL_BITS_LIMBS_64; ++i) {
                if (x.u64[i] != y.u64[i])
                        return false;
        }
        return true;
}

ASL_BITS_TYPE
ASL_eor_bits(N, int width, ASL_BITS_TYPE x, ASL_BITS_TYPE y)
{
        for (int i = 0; i < ASL_BITS_LIMBS_64; ++i)
             x.u64[i] ^= y.u64[i];
        return x;
}

ASL_BITS_TYPE
ASL_mk_mask(N, int width)
{
        return ASL_lsr_bits(N, N, ASL_bits_max(N), N - width);
}

ASL_BITS_TYPE
ASL_mul_bits(N, int width, ASL_BITS_TYPE x, ASL_BITS_TYPE y)
{
        ASL_BITS_TYPE r = ASL_zeros_bits(N, N);
        for (int i = 0; i < ASL_BITS_LIMBS_32; ++i) {
                uint64_t carry = 0;
                for (int j = 0; j < ASL_BITS_LIMBS_32 - i; ++j) {
                        uint64_t p = (uint64_t)r.u32[i + j]
                                     + (uint64_t)x.u32[i] * (uint64_t)y.u32[j]
                                     + carry;
                        carry = p >> 32;
                        r.u32[i + j] = p;
                }
        }
        return ASL_and_bits(N, width, r, ASL_mk_mask(N, width));
}

bool
ASL_ne_bits(N, int width, ASL_BITS_TYPE x, ASL_BITS_TYPE y)
{
        return !ASL_eq_bits(N, width, x, y);
}

ASL_BITS_TYPE
ASL_not_bits(N, int width, ASL_BITS_TYPE x)
{
        for (int i = 0; i < ASL_BITS_LIMBS_64; ++i)
             x.u64[i] = ~x.u64[i];
        return ASL_and_bits(N, width, x, ASL_mk_mask(N, width));
}

ASL_BITS_TYPE
ASL_ones_bits(N, int width)
{
        return ASL_mk_mask(N, width);
}

ASL_BITS_TYPE
ASL_or_bits(N, int width, ASL_BITS_TYPE x, ASL_BITS_TYPE y)
{
        for (int i = 0; i < ASL_BITS_LIMBS_64; ++i)
             x.u64[i] |= y.u64[i];
        return x;
}

ASL_BITS_TYPE
ASL_replicate_bits(N, N, int width, ASL_BITS_TYPE x, int n)
{
        ASL_BITS_TYPE r = ASL_zeros_bits(N, N);
        while (n-- > 0)
                r = ASL_or_bits(N, N, ASL_lsl_bits(N, N, r, width), x);
        return r;
}

ASL_BITS_TYPE
ASL_slice_lowd(N, N, ASL_BITS_TYPE x, int lo, int width)
{
        x = ASL_lsr_bits(N, N, x, lo);
        return ASL_and_bits(N, N, x, ASL_mk_mask(N, width));
}

ASL_BITS_TYPE
ASL_sub_bits(N, int width, ASL_BITS_TYPE x, ASL_BITS_TYPE y)
{
        ASL_BITS_TYPE r;
        uint64_t carry = 0;
        for (int i = 0; i < ASL_BITS_LIMBS_64; ++i) {
                 r.u64[i] = x.u64[i] - y.u64[i] - carry;
                 carry = (carry & (r.u64[i] == x.u64[i])) | (r.u64[i] > x.u64[i]);
        }
        return ASL_and_bits(N, width, r, ASL_mk_mask(N, width));
}

#undef ASL_BITS_LIMBS_64
#undef ASL_BITS_LIMBS_32
#undef ASL_BITS_TYPE
#undef ASL_INT_TYPE
