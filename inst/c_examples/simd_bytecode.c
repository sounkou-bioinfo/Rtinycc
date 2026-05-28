/*
 * TinyCC SIMD detection and selected opcode use through inline-assembly bytes.
 *
 * TinyCC's inline assembler does not understand every SIMD mnemonic on every
 * target. For small, fixed examples you can still emit exact instruction bytes
 * with `.byte`, as suggested on the tinycc-devel list. This is not a general
 * intrinsic implementation: the byte sequences below are x86-64 SysV ABI
 * examples that assume arguments arrive in RDI, RSI, and RDX.
 */

static void rtcc_cpuid(unsigned int leaf,
                       unsigned int subleaf,
                       unsigned int *a,
                       unsigned int *b,
                       unsigned int *c,
                       unsigned int *d) {
#if defined(__i386__) || defined(__x86_64__)
  unsigned int eax = leaf, ebx = 0, ecx = subleaf, edx = 0;
  __asm__ __volatile__("cpuid" : "+a"(eax), "=b"(ebx), "+c"(ecx), "=d"(edx));
  *a = eax;
  *b = ebx;
  *c = ecx;
  *d = edx;
#else
  (void)leaf;
  (void)subleaf;
  *a = *b = *c = *d = 0;
#endif
}

static unsigned long long rtcc_xgetbv0(void) {
#if defined(__i386__) || defined(__x86_64__)
  unsigned int eax = 0, edx = 0, ecx = 0;
  /* xgetbv: TinyCC may not know this mnemonic, so emit 0f 01 d0. */
  __asm__ __volatile__(".byte 0x0f, 0x01, 0xd0" : "=a"(eax), "=d"(edx) : "c"(ecx));
  return ((unsigned long long)edx << 32) | eax;
#else
  return 0;
#endif
}

int rtcc_cpu_has_sse2(void) {
#if defined(__i386__) || defined(__x86_64__)
  unsigned int a, b, c, d;
  rtcc_cpuid(1, 0, &a, &b, &c, &d);
  return (int)((d >> 26) & 1u);
#else
  return 0;
#endif
}

int rtcc_cpu_has_avx2(void) {
#if defined(__i386__) || defined(__x86_64__)
  unsigned int a, b, c, d;
  rtcc_cpuid(1, 0, &a, &b, &c, &d);
  if (((c >> 27) & 1u) == 0 || ((c >> 28) & 1u) == 0) {
    return 0;
  }
  if ((rtcc_xgetbv0() & 0x6u) != 0x6u) {
    return 0;
  }
  rtcc_cpuid(7, 0, &a, &b, &c, &d);
  return (int)((b >> 5) & 1u);
#else
  return 0;
#endif
}

int rtcc_cpu_has_avx512f_bw_vl(void) {
#if defined(__i386__) || defined(__x86_64__)
  unsigned int a, b, c, d;
  rtcc_cpuid(1, 0, &a, &b, &c, &d);
  if (((c >> 27) & 1u) == 0 || ((c >> 28) & 1u) == 0) {
    return 0;
  }
  if ((rtcc_xgetbv0() & 0xe6u) != 0xe6u) {
    return 0;
  }
  rtcc_cpuid(7, 0, &a, &b, &c, &d);
  return (int)(((b >> 16) & 1u) && ((b >> 30) & 1u) && ((b >> 31) & 1u));
#else
  return 0;
#endif
}

void rtcc_sse2_add4_i32(int *a, int *b, int *out) {
#if defined(__x86_64__) && !defined(_WIN32)
  (void)a;
  (void)b;
  (void)out;
  __asm__ __volatile__(
    ".byte 0xf3, 0x0f, 0x6f, 0x07\n\t" /* movdqu (%rdi), %xmm0 */
    ".byte 0xf3, 0x0f, 0x6f, 0x0e\n\t" /* movdqu (%rsi), %xmm1 */
    ".byte 0x66, 0x0f, 0xfe, 0xc1\n\t" /* paddd %xmm1, %xmm0 */
    ".byte 0xf3, 0x0f, 0x7f, 0x02\n\t" /* movdqu %xmm0, (%rdx) */
    :
    :
    : "memory"
  );
#else
  for (int i = 0; i < 4; ++i) {
    out[i] = a[i] + b[i];
  }
#endif
}

void rtcc_avx2_add8_i32(int *a, int *b, int *out) {
#if defined(__x86_64__) && !defined(_WIN32)
  (void)a;
  (void)b;
  (void)out;
  __asm__ __volatile__(
    ".byte 0xc5, 0xfe, 0x6f, 0x07\n\t" /* vmovdqu (%rdi), %ymm0 */
    ".byte 0xc5, 0xfe, 0x6f, 0x0e\n\t" /* vmovdqu (%rsi), %ymm1 */
    ".byte 0xc5, 0xfd, 0xfe, 0xc1\n\t" /* vpaddd %ymm1, %ymm0, %ymm0 */
    ".byte 0xc5, 0xfe, 0x7f, 0x02\n\t" /* vmovdqu %ymm0, (%rdx) */
    ".byte 0xc5, 0xf8, 0x77\n\t"       /* vzeroupper */
    :
    :
    : "memory"
  );
#else
  for (int i = 0; i < 8; ++i) {
    out[i] = a[i] + b[i];
  }
#endif
}
