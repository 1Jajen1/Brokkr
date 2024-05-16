#include <ModifiedUtf8.h>
#include <Cesu8.h>

int is_valid_modified_utf8(uint8_t const *const src, size_t len) {
  return is_valid_modified_utf8_fallback(src, len);
}

int is_valid_cesu8(uint8_t const *const src, size_t len) {
  return is_valid_cesu8_fallback(src, len);
}

// Useful for benchmarks

int is_valid_modified_utf8_branchy(uint8_t const *const src, size_t len) {
  return is_valid_modified_utf8_fallback(src, len);
}

int is_valid_cesu8_branchy(uint8_t const *const src, size_t len) {
  return is_valid_cesu8_fallback(src, len);
}

int is_valid_modified_utf8_simd(uint8_t const *const src, size_t len) {
  return is_valid_modified_utf8_fallback(src, len);
}

int is_valid_cesu8_simd(uint8_t const *const src, size_t len) {
  return is_valid_cesu8_fallback(src, len);
}
