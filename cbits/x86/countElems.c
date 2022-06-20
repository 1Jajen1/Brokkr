#include <stdint.h>
#include <stddef.h>
#include <immintrin.h>

// optimized for nibble arrays
// ~20-30x faster than generic avx since it does not have any inner loops
__attribute__((target("avx,avx2")))
static inline int countElems_4_avx(uint8_t const bitSz, size_t const dEls, uint8_t * const dArr, size_t const arrSz, uint64_t * const els, size_t const eSz) {
  size_t perWord = 64 / bitSz;
  
  uint8_t lutArr[32];
  
  for (size_t i = 0; i < 32; i++) {
    lutArr[i] = 0;
  }
  for (size_t i = 0; i < eSz; i++) {
    uint64_t el = els[i];
    if (el <= 15) {
      lutArr[els[i]] = (uint8_t)1;
      lutArr[els[i] + 16] = (uint8_t)1;
    }
  }
  __m256i const lut = _mm256_loadu_si256((__m256i *)lutArr);
  __m256i const loBitMask = _mm256_set1_epi8(15);

  size_t vecSz;
  if (arrSz * perWord == dEls && ((arrSz & 3) == 0)) 
    vecSz = arrSz >> 2;
  else
    vecSz = (arrSz - 1) >> 2;

  __m256i * bigPtrArr = (__m256i *)dArr;
  __m256i count4 = _mm256_setzero_si256();
  for (size_t i = 0; i < vecSz; i++) {
    __m256i w = _mm256_loadu_si256(bigPtrArr++);
    __m256i loRes = _mm256_shuffle_epi8(lut, _mm256_and_si256(w, loBitMask));
    __m256i hiRes = _mm256_shuffle_epi8(lut, _mm256_and_si256(_mm256_srli_epi16(w, 4), loBitMask));
    __m256i addRes = _mm256_add_epi8(loRes, hiRes);
    __m256i sadRes = _mm256_sad_epu8(addRes, _mm256_setzero_si256());
    count4 = _mm256_add_epi64(count4, sadRes);
  }

  // tail
  uint64_t singleMask = (((uint64_t) 1) << bitSz) - 1;
  int count =
    _mm256_extract_epi64(count4, 0) +
    _mm256_extract_epi64(count4, 1) +
    _mm256_extract_epi64(count4, 3) +
    _mm256_extract_epi64(count4, 2);
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

// --------------------- Generic counting
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
  int count = _mm256_extract_epi64(count4, 0) + _mm256_extract_epi64(count4, 1) + _mm256_extract_epi64(count4, 2) + _mm256_extract_epi64(count4, 3);
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

// Kept for reference and if someone wants to run on non-x86 with no simd stuff, this is fast enough so should be fine
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
  if (__builtin_cpu_supports("avx2")) {
    switch (bitSz) {
      case 4:
        return countElems_4_avx(bitSz, dEls, dArr, arrSz, els, eSz);  
      default:
        return countElemsGeneric_avx(bitSz, dEls, dArr, arrSz, els, eSz);
    }
  }
  return countElemsGeneric_sse(bitSz, dEls, dArr, arrSz, els, eSz);
}
