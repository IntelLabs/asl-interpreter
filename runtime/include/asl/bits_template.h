#define ASL_BITS_TYPE ASL_CC_INDIR(ASL_CC_INDIR(ASL_bits, N), _t)

ASL_BITS_TYPE ASL_not_bits(N, int width, ASL_BITS_TYPE x);

#undef ASL_BITS_TYPE
