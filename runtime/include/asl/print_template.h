#define ASL_BITS_LIMBS_64 (N >> 6)
#define ASL_BITS_TYPE ASL_CC_INDIR(ASL_CC_INDIR(ASL_bits, N), _t)

static inline void
ASL_print_bits_hex(N, int width, ASL_BITS_TYPE x)
{
        printf("%d'x", width);
        for (int i = ASL_BITS_LIMBS_64 - 1; i >= 0; --i)
                printf("%0llx", (long long)x.u64[i]);
}

#undef ASL_BITS_LIMBS_64
#undef ASL_BITS_TYPE
