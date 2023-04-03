#define ASL_BITS_TYPE ASL_CC_INDIR(ASL_CC_INDIR(ASL_bits, N), _t)
#define ASL_INT_TYPE ASL_CC_INDIR(ASL_CC_INDIR(ASL_int, N), _t)

ASL_BITS_TYPE ASL_add_bits(N, int width, ASL_BITS_TYPE x, ASL_BITS_TYPE y);
ASL_BITS_TYPE ASL_and_bits(N, int width, ASL_BITS_TYPE x, ASL_BITS_TYPE y);
ASL_BITS_TYPE ASL_asr_bits(N, int width, ASL_BITS_TYPE x, int d);
ASL_INT_TYPE ASL_cvt_bits_sint(N, int width, ASL_BITS_TYPE x);
ASL_INT_TYPE ASL_cvt_bits_uint(N, int width, ASL_BITS_TYPE x);
ASL_BITS_TYPE ASL_cvt_int_bits(N, int width, ASL_INT_TYPE x);
ASL_BITS_TYPE ASL_eor_bits(N, int width, ASL_BITS_TYPE x, ASL_BITS_TYPE y);
bool ASL_eq_bits(N, int width, ASL_BITS_TYPE x, ASL_BITS_TYPE y);
ASL_BITS_TYPE ASL_mk_mask(N, int width);
bool ASL_ne_bits(N, int width, ASL_BITS_TYPE x, ASL_BITS_TYPE y);
ASL_BITS_TYPE ASL_mul_bits(N, int width, ASL_BITS_TYPE x, ASL_BITS_TYPE y);
ASL_BITS_TYPE ASL_not_bits(N, int width, ASL_BITS_TYPE x);
ASL_BITS_TYPE ASL_ones_bits(N, int width);
ASL_BITS_TYPE ASL_or_bits(N, int width, ASL_BITS_TYPE x, ASL_BITS_TYPE y);
ASL_BITS_TYPE ASL_replicate_bits(N, int width, ASL_BITS_TYPE x, int n);
ASL_BITS_TYPE ASL_slice_lowd(N, N, ASL_BITS_TYPE x, int lo, int width);
ASL_BITS_TYPE ASL_sub_bits(N, int width, ASL_BITS_TYPE x, ASL_BITS_TYPE y);
ASL_BITS_TYPE ASL_zero_extend_bits(N, N, int width, ASL_BITS_TYPE x, int n);

#undef ASL_BITS_TYPE
#undef ASL_INT_TYPE
