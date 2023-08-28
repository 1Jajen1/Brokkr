#include <stdint.h>
#include <stddef.h>
#include <immintrin.h>

// 16 bit
static inline void vec_bswap16_fallback(uint16_t * const from, uint16_t * to, size_t const sz) {
  for (size_t i = 0; i < sz; i++) {
    to[i] = __builtin_bswap16(from[i]);
  }
}

// avx512
__attribute__((target("avx512bw")))
static inline int vec_bswap16_avx512(uint16_t * const from, uint16_t * to, size_t const sz) { 
  size_t simdSz = sz >> 5;
  size_t tailSz = sz & 31;

  __m512i const shuffle = _mm512_set_epi8(
      1, 0, 3, 2, 5, 4, 7, 6, 9, 8, 11, 10, 13, 12, 15, 14,
      1, 0, 3, 2, 5, 4, 7, 6, 9, 8, 11, 10, 13, 12, 15, 14,
      1, 0, 3, 2, 5, 4, 7, 6, 9, 8, 11, 10, 13, 12, 15, 14,
      1, 0, 3, 2, 5, 4, 7, 6, 9, 8, 11, 10, 13, 12, 15, 14);

  __m512i * bigPtrFrom = (__m512i *)from;
  __m512i * bigPtrTo = (__m512i *)from;
  for (size_t i = 0; i < simdSz; i++) {
    _mm512_storeu_si512(bigPtrTo++, _mm512_shuffle_epi8(_mm512_loadu_si512(bigPtrFrom++), shuffle));
  }

  vec_bswap16_fallback((uint16_t * const) bigPtrFrom, (uint16_t *) bigPtrTo, tailSz);
}

// avx256
__attribute__((target("avx2")))
static inline int vec_bswap16_avx256(uint16_t * const from, uint16_t * to, size_t const sz) { 
  size_t simdSz = sz >> 4;
  size_t tailSz = sz & 15;

  __m256i const shuffle = _mm256_set_epi8(
      1, 0, 3, 2, 5, 4, 7, 6, 9, 8, 11, 10, 13, 12, 15, 14,
      1, 0, 3, 2, 5, 4, 7, 6, 9, 8, 11, 10, 13, 12, 15, 14);

  __m256i * bigPtrFrom = (__m256i *)from;
  __m256i * bigPtrTo = (__m256i *)from;
  for (size_t i = 0; i < simdSz; i++) {
    _mm256_storeu_si256(bigPtrTo++, _mm256_shuffle_epi8(_mm256_loadu_si256(bigPtrFrom++), shuffle));
  }

  vec_bswap16_fallback((uint16_t * const) bigPtrFrom, (uint16_t *) bigPtrTo, tailSz);
}

void vec_bswap16(uint16_t * const from, uint16_t * to, size_t const sz) {
  __builtin_cpu_init();

  if (__builtin_cpu_supports("avx512bw")) {
    vec_bswap16_avx512(from, to, sz);
  } else if (__builtin_cpu_supports("avx2")) {
    vec_bswap16_avx256(from, to, sz);
  } else {
    vec_bswap16_fallback(from, to, sz);
  }
}

// 32 bit
static inline void vec_bswap32_fallback(uint32_t * const from, uint32_t * to, size_t const sz) {
  for (size_t i = 0; i < sz; i++) {
    to[i] = __builtin_bswap32(from[i]);
  }
}

// avx512
__attribute__((target("avx512bw")))
static inline int vec_bswap32_avx512(uint32_t * const from, uint32_t * to, size_t const sz) { 
  size_t simdSz = sz >> 4;
  size_t tailSz = sz & 15;

  __m512i const shuffle = _mm512_set_epi8(
      3, 2, 1, 0, 7, 6, 5, 4, 11, 10, 9, 8, 15, 14, 13, 12,
      3, 2, 1, 0, 7, 6, 5, 4, 11, 10, 9, 8, 15, 14, 13, 12,
      3, 2, 1, 0, 7, 6, 5, 4, 11, 10, 9, 8, 15, 14, 13, 12,
      3, 2, 1, 0, 7, 6, 5, 4, 11, 10, 9, 8, 15, 14, 13, 12);

  __m512i * bigPtrFrom = (__m512i *)from;
  __m512i * bigPtrTo = (__m512i *)from;
  for (size_t i = 0; i < simdSz; i++) {
    _mm512_storeu_si512(bigPtrTo++, _mm512_shuffle_epi8(_mm512_loadu_si512(bigPtrFrom++), shuffle));
  }

  vec_bswap32_fallback((uint32_t * const) bigPtrFrom, (uint32_t *) bigPtrTo, tailSz);
}

// avx256
__attribute__((target("avx2")))
static inline int vec_bswap32_avx256(uint32_t * const from, uint32_t * to, size_t const sz) { 
  size_t simdSz = sz >> 3;
  size_t tailSz = sz & 7;

  __m256i const shuffle = _mm256_set_epi8(
      3, 2, 1, 0, 7, 6, 5, 4, 11, 10, 9, 8, 15, 14, 13, 12,
      3, 2, 1, 0, 7, 6, 5, 4, 11, 10, 9, 8, 15, 14, 13, 12);

  __m256i * bigPtrFrom = (__m256i *)from;
  __m256i * bigPtrTo = (__m256i *)from;
  for (size_t i = 0; i < simdSz; i++) {
    _mm256_storeu_si256(bigPtrTo++, _mm256_shuffle_epi8(_mm256_loadu_si256(bigPtrFrom++), shuffle));
  }

  vec_bswap32_fallback((uint32_t * const) bigPtrFrom, (uint32_t *) bigPtrTo, tailSz);
}

void vec_bswap32(uint32_t * const from, uint32_t * to, size_t const sz) {
  __builtin_cpu_init();

  if (__builtin_cpu_supports("avx512bw")) {
    vec_bswap32_avx512(from, to, sz);
  } else if (__builtin_cpu_supports("avx2")) {
    vec_bswap32_avx256(from, to, sz);
  } else {
    vec_bswap32_fallback(from, to, sz);
  }
}

// 64 bit
static inline void vec_bswap64_fallback(uint64_t * const from, uint64_t * to, size_t const sz) {
  for (size_t i = 0; i < sz; i++) {
    to[i] = __builtin_bswap64(from[i]);
  }
}

// avx512
__attribute__((target("avx512bw")))
static inline int vec_bswap64_avx512(uint64_t * const from, uint64_t * to, size_t const sz) { 
  size_t simdSz = sz >> 3;
  size_t tailSz = sz & 7;

  __m512i const shuffle = _mm512_set_epi8(
      7, 6, 5, 4, 3, 2, 1, 0, 15, 14, 13, 12, 11, 10, 9, 8,
      7, 6, 5, 4, 3, 2, 1, 0, 15, 14, 13, 12, 11, 10, 9, 8,
      7, 6, 5, 4, 3, 2, 1, 0, 15, 14, 13, 12, 11, 10, 9, 8,
      7, 6, 5, 4, 3, 2, 1, 0, 15, 14, 13, 12, 11, 10, 9, 8);

  __m512i * bigPtrFrom = (__m512i *)from;
  __m512i * bigPtrTo = (__m512i *)from;
  for (size_t i = 0; i < simdSz; i++) {
    _mm512_storeu_si512(bigPtrTo++, _mm512_shuffle_epi8(_mm512_loadu_si512(bigPtrFrom++), shuffle));
  }

  vec_bswap64_fallback((uint64_t * const) bigPtrFrom, (uint64_t *) bigPtrTo, tailSz);
}

// avx256
__attribute__((target("avx2")))
static inline int vec_bswap64_avx256(uint64_t * const from, uint64_t * to, size_t const sz) { 
  size_t simdSz = sz >> 2;
  size_t tailSz = sz & 3;

  __m256i const shuffle = _mm256_set_epi8(
      7, 6, 5, 4, 3, 2, 1, 0, 15, 14, 13, 12, 11, 10, 9, 8,
      7, 6, 5, 4, 3, 2, 1, 0, 15, 14, 13, 12, 11, 10, 9, 8);

  __m256i * bigPtrFrom = (__m256i *)from;
  __m256i * bigPtrTo = (__m256i *)from;
  for (size_t i = 0; i < simdSz; i++) {
    _mm256_storeu_si256(bigPtrTo++, _mm256_shuffle_epi8(_mm256_loadu_si256(bigPtrFrom++), shuffle));
  }
  
  vec_bswap64_fallback((uint64_t * const) bigPtrFrom, (uint64_t *) bigPtrTo, tailSz);
}

void vec_bswap64(uint64_t * const from, uint64_t * to, size_t const sz) {
  __builtin_cpu_init();

  if (__builtin_cpu_supports("avx512bw")) {
    vec_bswap64_avx512(from, to, sz);
  } else if (__builtin_cpu_supports("avx2")) {
    vec_bswap64_avx256(from, to, sz);
  } else {
    vec_bswap64_fallback(from, to, sz);
  }
}
