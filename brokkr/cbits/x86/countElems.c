#include <stdint.h>
#include <stddef.h>
#include <immintrin.h>

__attribute__((target("avx512f,avx512bw")))
static inline int countElems_4_avx512(uint8_t const bitSz, size_t const dEls, uint8_t * const dArr, size_t const arrSz, uint64_t * const els, size_t const eSz) {  
  uint8_t lutArr[64];
  
  for (size_t i = 0; i < 64; i++) {
    lutArr[i] = 0;
  }
  for (size_t i = 0; i < eSz; i++) {
    uint64_t el = els[i];
    // This is merely a sanity check. el should never be >15 because el is used as an element in the 4 bit array
    if (el <= 15) {
      lutArr[el] = (uint8_t)1;
      lutArr[el + 16] = (uint8_t)1;
      lutArr[el + 32] = (uint8_t)1;
      lutArr[el + 48] = (uint8_t)1;
    }
  }
  __m512i const lut = _mm512_loadu_si512((__m512i *)lutArr);
  __m512i const loBitMask = _mm512_set1_epi8(15);

  __m512i * bigPtrArr = (__m512i *)dArr;
  __m512i count8 = _mm512_setzero_si512();
  for (size_t i = 0; i < arrSz >> 3; i++) {
    __m512i w = _mm512_loadu_si512(bigPtrArr++);
    __m512i loRes = _mm512_shuffle_epi8(lut, _mm512_and_si512(w, loBitMask));
    __m512i hiRes = _mm512_shuffle_epi8(lut, _mm512_and_si512(_mm512_srli_epi16(w, 4), loBitMask));
    __m512i addRes = _mm512_add_epi8(loRes, hiRes);
    __m512i sadRes = _mm512_sad_epu8(addRes, _mm512_setzero_si512());
    count8 = _mm512_add_epi64(count8, sadRes);
  }

  return _mm512_reduce_add_epi64(count8);
}

__attribute__((target("avx,avx2")))
static inline int countElems_4_avx(uint8_t const bitSz, size_t const dEls, uint8_t * const dArr, size_t const arrSz, uint64_t * const els, size_t const eSz) {  
  uint8_t lutArr[32];
  
  for (size_t i = 0; i < 32; i++) {
    lutArr[i] = 0;
  }
  for (size_t i = 0; i < eSz; i++) {
    uint64_t el = els[i];
    // This is merely a sanity check. el should never be >15 because el is used as an element in the 4 bit array
    if (el <= 15) {
      lutArr[el] = (uint8_t)1;
      lutArr[el + 16] = (uint8_t)1;
    }
  }
  __m256i const lut = _mm256_loadu_si256((__m256i *)lutArr);
  __m256i const loBitMask = _mm256_set1_epi8(15);

  __m256i * bigPtrArr = (__m256i *)dArr;
  __m256i count4 = _mm256_setzero_si256();
  for (size_t i = 0; i < arrSz >> 2; i++) {
    __m256i w = _mm256_loadu_si256(bigPtrArr++);
    __m256i loRes = _mm256_shuffle_epi8(lut, _mm256_and_si256(w, loBitMask));
    __m256i hiRes = _mm256_shuffle_epi8(lut, _mm256_and_si256(_mm256_srli_epi16(w, 4), loBitMask));
    __m256i addRes = _mm256_add_epi8(loRes, hiRes);
    __m256i sadRes = _mm256_sad_epu8(addRes, _mm256_setzero_si256());
    count4 = _mm256_add_epi64(count4, sadRes);
  }
  
  __m128i res = _mm256_extracti128_si256(count4, 0);
  res = _mm_add_epi64(res, _mm256_extracti128_si256(count4, 1));
  return _mm_extract_epi64(res, 0) + _mm_extract_epi64(res, 1);
}

// --------------------- Generic counting (arbitrary bitsize)
// 512 bit. This turns out to be slower than the 256 bit version, I should probably investigate at some point why...
__attribute__((target("avx512f,avx512dq")))
static inline int countElemsGeneric_avx512(uint8_t const bitSz, size_t const dEls, uint8_t * const dArr, size_t const arrSz, uint64_t * const els, size_t const eSz) {
  uint64_t singleMask = (((uint64_t) 1) << bitSz) - 1;
  __m512i mask = _mm512_set1_epi64(singleMask);
  size_t perWord = 64 / bitSz;
  __m512i count8 = _mm512_setzero_si512();
  
  size_t vecSz;
  if (arrSz * perWord == dEls && ((arrSz & 3) == 0)) 
    vecSz = arrSz >> 2;
  else
    vecSz = (arrSz - 1) >> 2;
  
  __m512i * bigPtrArr = (__m512i *)dArr;
  __m512i one = _mm512_set1_epi64(1);
  for (size_t i = 0; i < vecSz; i++) {
    __m512i w = _mm512_loadu_si512(bigPtrArr++);
    for (size_t j = 0; j < perWord; j++) {
      __m512i el = _mm512_and_si512(w, mask);
      for (size_t z = 0; z < eSz; z++) {
        uint64_t toCmp = els[z];
        __m512i cmp = _mm512_set1_epi64(toCmp);
        __mmask8 cmpRes = _mm512_cmpeq_epi64_mask(el, cmp);
        __m512i res = _mm512_and_si512(_mm512_movm_epi64(cmpRes), one);
        count8 = _mm512_add_epi64(count8, res);
      }
      w = _mm512_srli_epi64(w, bitSz);
    }
  }

  // tail
  
  int count = _mm512_reduce_add_epi64(count8);
  size_t currPos = vecSz * 4;
  size_t visited = currPos * perWord;
  for (size_t i = currPos; i < arrSz; i++) {
    uint64_t w = ((uint64_t *)dArr)[i];
    for (size_t j = 0; j < perWord; j++) {
      uint64_t el = w & singleMask;
      for (size_t z = 0; z < eSz; z++) {
        if (el == els[z]) {
          count++;
          break;
        }
      }
      if (++visited >= dEls) return count;
      w = w >> bitSz;
    }
  }
  return count;
}

// 256 bits
__attribute__((target("avx,avx2")))
static inline int countElemsGeneric_avx(uint8_t const bitSz, size_t const dEls, uint8_t * const dArr, size_t const arrSz, uint64_t * const els, size_t const eSz) {
  uint64_t singleMask = (((uint64_t) 1) << bitSz) - 1;
  __m256i mask = _mm256_set1_epi64x(singleMask);
  size_t perWord = 64 / bitSz;
  __m256i count4 = _mm256_setzero_si256();
  
  size_t vecSz;
  if (arrSz * perWord == dEls && ((arrSz & 3) == 0)) 
    vecSz = arrSz >> 2;
  else
    vecSz = (arrSz - 1) >> 2;
  
  __m256i * bigPtrArr = (__m256i *)dArr;
  __m256i one = _mm256_set1_epi64x(1);
  for (size_t i = 0; i < vecSz; i++) {
    __m256i w = _mm256_loadu_si256(bigPtrArr++);
    for (size_t j = 0; j < perWord; j++) {
      __m256i el = _mm256_and_si256(w, mask);
      for (size_t z = 0; z < eSz; z++) {
        uint64_t toCmp = els[z];
        __m256i cmp = _mm256_set1_epi64x(toCmp);
        __m256i cmpRes = _mm256_cmpeq_epi64(el, cmp);
        __m256i res = _mm256_and_si256(cmpRes, one);
        count4 = _mm256_add_epi64(count4, res);
      }
      w = _mm256_srli_epi64(w, bitSz);
    }
  }

  // tail
  __m128i res = _mm256_extracti128_si256(count4, 0);
  res = _mm_add_epi64(res, _mm256_extracti128_si256(count4, 1));
  int count = _mm_extract_epi64(res, 0) + _mm_extract_epi64(res, 1);

  size_t currPos = vecSz * 4;
  size_t visited = currPos * perWord;
  for (size_t i = currPos; i < arrSz; i++) {
    uint64_t w = ((uint64_t *)dArr)[i];
    for (size_t j = 0; j < perWord; j++) {
      uint64_t el = w & singleMask;
      for (size_t z = 0; z < eSz; z++) {
        if (el == els[z]) {
          count++;
          break;
        }
      }
      if (++visited >= dEls) return count;
      w = w >> bitSz;
    }
  }
  return count;
}

// 128 bit
static inline int countElemsGeneric_sse(uint8_t const bitSz, size_t const dEls, uint8_t * const dArr, size_t const arrSz, uint64_t * const els, size_t const eSz) {
  uint64_t singleMask = (((uint64_t) 1) << bitSz) - 1;
  __m128i mask = _mm_set1_epi64x(singleMask);
  size_t perWord = 64 / bitSz;
  __m128i count2 = _mm_setzero_si128();
  
  size_t vecSz;
  if (arrSz * perWord == dEls && ((arrSz & 2) == 0)) 
    vecSz = arrSz >> 1;
  else
    vecSz = (arrSz - 1) >> 1;

  __m128i * bigPtrArr = (__m128i *)dArr;
  __m128i one = _mm_set1_epi32(1);
  __m128i lowMask = _mm_set1_epi64x(0xFFFFFFFF);
  for (size_t i = 0; i < vecSz; i++) {
    __m128i w = _mm_loadu_si128(bigPtrArr++);
    for (size_t j = 0; j < perWord; j++) {
      __m128i el = _mm_and_si128(w, mask);
      for (size_t z = 0; z < eSz; z++) {
        uint64_t toCmp = els[z];
        __m128i cmp = _mm_set1_epi64x(toCmp);
        __m128i cmpRes = _mm_cmpeq_epi32(el, cmp);
        __m128i res1 = _mm_and_si128(cmpRes, one);
        __m128i res2 = _mm_and_si128(res1, _mm_and_si128(_mm_srli_si128(res1, 4), lowMask));
        count2 = _mm_add_epi64(count2, res2);
      }
      w = _mm_srli_epi64(w, bitSz);
    }
  }

  // tail
  int count = _mm_cvtsi128_si64(count2) + _mm_cvtsi128_si64(_mm_srli_si128(count2, 8));
  size_t currPos = vecSz * 2;
  size_t visited = currPos * perWord;
  for (size_t i = currPos; i < arrSz; i++) {
    uint64_t w = ((uint64_t *)dArr)[i];
    for (size_t j = 0; j < perWord; j++) {
      uint64_t el = w & singleMask;
      for (size_t z = 0; z < eSz; z++) {
        if (el == els[z]) {
          count++;
          break;
        }
      }
      if (++visited >= dEls) return count;
      w = w >> bitSz;
    }
  }
  return count;
}

// Kept for reference and if someone wants to run on non-x86 or without simd, this is fast enough so should be fine
static inline int countElemsGeneric_fallback(uint8_t const bitSz, size_t const dEls, uint8_t * const dArr, size_t const arrSz, uint64_t * const els, size_t const eSz) {
  uint64_t mask = (((uint64_t)1) << bitSz) - 1;
  size_t perWord = 64 / bitSz;
  int count = 0;
  size_t visited = 0;
  for (size_t i = 0; i < arrSz; i++) {
    uint64_t w = ((uint64_t *)dArr)[i];
    for (size_t j = 0; j < perWord; j++) {
      uint64_t el = w & mask;
      for (size_t z = 0; z < eSz; z++) {
        if (el == els[z]) {
          count++;
          break;
        }
      }
      if (++visited >= dEls) return count;
      w = w >> bitSz;
    }
  }
  return count;
}

int countElems(uint8_t const bitSz, size_t const dEls, uint8_t * const dArr, size_t const arrSz, uint64_t * const els, size_t const eSz) {
  __builtin_cpu_init();
  
  if (bitSz == 4 && __builtin_cpu_supports("avx512f") && __builtin_cpu_supports("avx512bw")) {
    return countElems_4_avx512(bitSz, dEls, dArr, arrSz, els, eSz);
  }

  // if (__builtin_cpu_supports("avx512f") && __builtin_cpu_supports("avx512dq")) {
  //   return countElemsGeneric_avx512(bitSz, dEls, dArr, arrSz, els, eSz);
  // }

  if (__builtin_cpu_supports("avx2") && __builtin_cpu_supports("avx2")) {
    switch (bitSz) {
      case 4:
        return countElems_4_avx(bitSz, dEls, dArr, arrSz, els, eSz);  
      default:
        return countElemsGeneric_avx(bitSz, dEls, dArr, arrSz, els, eSz);
    }
  }
  return countElemsGeneric_sse(bitSz, dEls, dArr, arrSz, els, eSz);
}
