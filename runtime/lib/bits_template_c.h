#define ASL_BITS_CHUNKS N >> 6
#define ASL_BITS_TYPE ASL_CC_INDIR(ASL_CC_INDIR(ASL_bits, N), _t)

ASL_BITS_TYPE
ASL_not_bits(N, int width, ASL_BITS_TYPE x)
{
        for (int i = 0; i < ASL_BITS_CHUNKS; ++i)
             x.v[i] = ~x.v[i];
        return ASL_and_bits(N, x, ASL_mk_mask(N, width));
}

#undef ASL_BITS_CHUNKS
#undef ASL_BITS_TYPE
