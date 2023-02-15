#define ASL_BITS_CHUNKS N >> 6
#define ASL_BITS_TYPE ASL_CC_INDIR(ASL_CC_INDIR(ASL_bits, N), _t)

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
ASL_not_bits(N, int width, ASL_BITS_TYPE x)
{
        for (int i = 0; i < ASL_BITS_CHUNKS; ++i)
             x.v[i] = ~x.v[i];
        return ASL_and_bits(N, x, ASL_mk_mask(N, width));
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
