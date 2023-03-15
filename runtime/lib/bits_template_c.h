#define ASL_BITS_CHUNKS N >> 6
#define ASL_BITS_TYPE ASL_CC_INDIR(ASL_CC_INDIR(ASL_bits, N), _t)
#define ASL_INT_TYPE ASL_CC_INDIR(ASL_CC_INDIR(ASL_int, N), _t)

ASL_BITS_TYPE
ASL_and_bits(N, int width, ASL_BITS_TYPE x, ASL_BITS_TYPE y)
{
        for (int i = 0; i < ASL_BITS_CHUNKS; ++i)
             x.v[i] &= y.v[i];
        return x;
}

ASL_INT_TYPE
ASL_cvt_bits_uint(N, int width, ASL_BITS_TYPE x)
{
        ASL_INT_TYPE r;
        for (int i = 0; i < ASL_BITS_CHUNKS; ++i)
             r.v[i] = x.v[i];
        return r;
}

ASL_BITS_TYPE
ASL_cvt_int_bits(N, int width, ASL_INT_TYPE x)
{
        ASL_BITS_TYPE r;
        for (int i = 0; i < ASL_BITS_CHUNKS; ++i)
             r.v[i] = x.v[i];
        return ASL_and_bits(N, width, r, ASL_mk_mask(N, width));
}

bool
ASL_eq_bits(N, int width, ASL_BITS_TYPE x, ASL_BITS_TYPE y)
{
        for (int i = 0; i < ASL_BITS_CHUNKS; ++i) {
                if (x.v[i] != y.v[i])
                        return false;
        }
        return true;
}

ASL_BITS_TYPE
ASL_eor_bits(N, int width, ASL_BITS_TYPE x, ASL_BITS_TYPE y)
{
        for (int i = 0; i < ASL_BITS_CHUNKS; ++i)
             x.v[i] ^= y.v[i];
        return x;
}

ASL_BITS_TYPE
ASL_mk_mask(N, int width)
{
        return ASL_lsr_bits(N, N, ASL_bits_max(N), N - width);
}

bool
ASL_ne_bits(N, int width, ASL_BITS_TYPE x, ASL_BITS_TYPE y)
{
        return !ASL_eq_bits(N, width, x, y);
}

ASL_BITS_TYPE
ASL_not_bits(N, int width, ASL_BITS_TYPE x)
{
        for (int i = 0; i < ASL_BITS_CHUNKS; ++i)
             x.v[i] = ~x.v[i];
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
        for (int i = 0; i < ASL_BITS_CHUNKS; ++i)
             x.v[i] |= y.v[i];
        return x;
}

#undef ASL_BITS_CHUNKS
#undef ASL_BITS_TYPE
#undef ASL_INT_TYPE
