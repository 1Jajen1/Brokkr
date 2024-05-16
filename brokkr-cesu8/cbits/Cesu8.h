#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>

#define likely(x)      __builtin_expect(!!(x), 1) 
#define unlikely(x)    __builtin_expect(!!(x), 0) 

/*

Rules for valid modified utf-8 compared to utf-8

- Null is overlong 2 byte sequence
- No 4 byte format. Uses surrogate pairs

Heavily inspired by Koz Ross and the fallback utf-8 validation in https://github.com/haskell/bytestring

TODO State machine based version and simd version
*/

#include <MachDeps.h>

#ifdef WORDS_BIGENDIAN
#define to_little_endian(x) __builtin_bswap64(x)
#else
#define to_little_endian(x) (x)
#endif

// 0x80 in every 'lane'.
static uint64_t const high_bits_mask = 0x8080808080808080ULL;
static uint64_t const low_bits_mask  = 0x7F7F7F7F7F7F7F7FULL;

// unaligned 64 bit reads
static inline uint64_t read_uint64(const uint64_t *p) {
  uint64_t r;
  memcpy(&r, p, 8);
  return r;
}

static inline int is_valid_cesu8_fallback(uint8_t const *const src, size_t len) {
  uint8_t const *ptr = (uint8_t const *)src;

  uint8_t const *const end = ptr + len;
  while (likely(ptr < end)) {
    uint8_t const byte = *ptr;
    // 1 byte. Highest bit == 0
    if (likely(byte < 0b10000000)) {
      ptr++;

      // verify a block of bytes. Validates up to the first non-ascii byte
      if (likely(ptr + 8 < end && true)) {
        uint64_t const *big_ptr = (uint64_t const *)ptr;
        uint64_t word = to_little_endian(read_uint64(big_ptr));

        // look for non-ascii
        uint64_t validAscii = word & high_bits_mask;
        if (validAscii == 0) {
          ptr += 8;
        } else {
          ptr += __builtin_ctzl(validAscii) >> 3;
        }
      }
    }
    // maybe 2 bytes?
    // First byte 0b110XXXXX
    // Range of 0b11000000 to 0b11011111
    // Tho technically the range starts at 0b11000010 because utf-8 disallows overlong encodings
    // While this is true for utf-8, it isn't true for modified-utf, because null is encoded as a
    //  two byte overlong sequence, so we also have to check for that. The null byte is [192,128]
    // Second byte 0b10XXXXXX
    // Range of 0b10000000 to 0b10111111
    // Here we can simply cast to signed and use that (int8_t)128 = -128 < (int8_t) 191 = -64 < 0
    else if (ptr + 1 < end &&
        // Check if both bytes are valid utf-8
        (byte >= 0b11000010 && byte <= 0b11011111)) {
          uint8_t const byte2 = *(ptr + 1);
          bool byte2Valid = byte2 >= 0b10000000 && byte2 <= 0b10111111;
          if (!byte2Valid) return 0;
          ptr += 2;
    }
    // maybe 3 bytes without surrogates?
    // First byte is 0b1110XXXX
    // Range of 0b11100000 to 0b11101111
    // Second and third bytes are 0b10XXXXXX
    else if (ptr + 2 < end && byte >= 0b11100000 && byte <= 0b11101111) {
      uint8_t const byte2 = *(ptr + 1);
      uint8_t const byte3 = *(ptr + 2);
      bool byte2Valid = byte2 >= 0b10000000 && byte2 <= 0b10111111;
      bool byte3Valid = byte3 >= 0b10000000 && byte3 <= 0b10111111;
      // first accept all 3 byte sequences that are not surrogates
      if (byte2Valid && byte3Valid &&
          // check if overlong
          ((byte == 0b11100000 && byte2 >= 0b10100000) ||
          // first byte below the surrogate range and not overlong
          (byte >= 0b11100001 && byte  <= 0b11101100) ||
          // first byte indicates surrogates, check next byte
          // surrogates will be validated later, so only accept
          // non-surrogates here
          (byte == 0b11101101 && byte2 <= 0b10011111) ||
          // above the surrogate range
          (byte >= 0b11101110 && byte  <= 0b11101111))) {
        ptr += 3;
      }
      // check for surrogate pairs
      else if (ptr + 5 < end && byte == 0b11101101) {
        bool byte2Valid = 0b10100000 <= byte2 && byte2 <= 0b10101111;
        bool byte4Valid = *(ptr + 3) == 0b11101101;
        uint8_t const byte5 = *(ptr + 4);
        bool byte5Valid = 0b10110000 <= byte5 && byte5 <= 0b10111111;
        bool byte6Valid = (* (ptr + 5)) <= 0b10111111;
        if (byte2Valid && byte3Valid && byte4Valid && byte5Valid) {
          ptr += 6;
        } else {
          // invalid surrogate pair
          // printf("Invalid surrogate\n");
          return 0;
        }
      }
      // invalid 3 byte sequence or missing the second half of a surrogate pair
      else {
        // printf("Invalid 3 byte or missing second half surrogate\n");
        return 0;
      }
    }
    // everything here is invalid
    else {
      // printf("Invalid else %i", byte);
      return 0;
    }
  }
  // we are done and valid
  return 1;
}
