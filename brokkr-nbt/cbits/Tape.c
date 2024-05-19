#include <stdint.h>
#include <stddef.h>

#define MAX_DEPTH 1024 // This ought to be enough for everyone

int parseIntoTape(const uint8_t *data, const size_t len, const int network, uint64_t * tape) {
  const uint8_t * end = data + len;
  const uint64_t * tapeStart = tape;

  if (end == data) goto error;
  const uint8_t tag = *data++;

  if (!network) {
    const size_t rem = end - data;
    if (rem < 2) goto error;
    const size_t sz = __builtin_bswap16(*((uint16_t *) data));
    if (rem - 2 < sz) goto error; 
    data += sz + 2;
  }

  void* contStackArr[MAX_DEPTH];
  void** contStack = contStackArr;
  void* cont = &&success;

  uint64_t* tapeElsArr[MAX_DEPTH];
  uint64_t** tapeEls = tapeElsArr;

  size_t listCounterArr[MAX_DEPTH];
  size_t* listCounter = listCounterArr;

  const void* labels[] = {
    &&error, &&tagByte, &&tagShort, &&tagInt, &&tagLong, &&tagFloat, &&tagDouble,
    &&tagByteArray, &&tagString, &&tagList, &&tagCompound, &&tagIntArray, &&tagLongArray
    };
  
  if (tag > 12) goto error;
  goto *labels[tag];

tagByte:
  if (end == data) goto error;
  *tape++ = ((uint64_t)1 << 56) | *data++;
  goto *cont;
tagShort:
  if (end - data < 2) goto error; 
  *tape++ = ((uint64_t)2 << 56) | __builtin_bswap16(*((uint16_t *) data));
  data += 2;
  goto *cont;
tagInt: {
  if (end - data < 4) goto error; 
  *tape++ = ((uint64_t)3 << 56) | __builtin_bswap32(*((uint32_t *) data));
  data += 4;
  goto *cont;
}
tagLong: {
  if (end - data < 8) goto error;
  *tape++ = ((uint64_t)4 << 56);
  *tape++ = __builtin_bswap64(*((uint64_t *) data));
  data += 8;
  goto *cont;
}
tagFloat: {
  if (end - data < 4) goto error; 
  *tape++ = ((uint64_t)5 << 56) | __builtin_bswap32(*((uint32_t *) data));
  data += 4;
  goto *cont;
}
tagDouble: {
  if (end - data < 8) goto error; 
  *tape++ = ((uint64_t)6 << 56);
  *tape++ = __builtin_bswap64(*((uint64_t *) data));
  data += 8;
  goto *cont;
}
tagString: {
  if (end - data < 2) goto error;
  const size_t sz = __builtin_bswap16(*((uint16_t *) data));
  if (end - data - 2 < sz) goto error;
  *tape++ = ((uint64_t)8 << 56) | (sz << 32) | (end - data);
  data += sz + 2;
  goto *cont;
}
tagByteArray: {
  if (end - data < 4) goto error;
  const size_t sz = __builtin_bswap32(*((uint32_t *) data));
  if (end - data - 4 < sz) goto error;
  *tape++ = ((uint64_t)7 << 56) | (tape - tapeStart);
  data += sz + 4;
  goto *cont;
}
tagIntArray: {
  if (end - data < 4) goto error;
  const size_t sz = __builtin_bswap32(*((uint32_t *) data));
  if (end - data - 4 < sz * 4) goto error;
  *tape++ = ((uint64_t)11 << 56) | (end - data);
  data += sz * 4 + 4;
  goto *cont;
}
tagLongArray: {
  if (end - data < 4) goto error;
  const size_t sz = __builtin_bswap32(*((uint32_t *) data));
  if (end - data - 4 < sz * 8) goto error;
  *tape++ = ((uint64_t)12 << 56) | (end - data);
  data += sz * 8 + 4;
  goto *cont;
}
tagList: {
  if (contStack - contStackArr == MAX_DEPTH) goto error;
  if (end - data < 5) goto error;
  const uint8_t tag = *data++;
  size_t sz = __builtin_bswap32(*((uint32_t *) data));
  data += 4;
  *tape++ = ((uint64_t)9 << 56) | ((uint64_t)tag << 48) | (end - data);
  if (tag - 1 < 6) {
    const uint32_t szs = 0x323210;
    data += sz << (0xF & (szs >> (tag - 1) * 4));
  } else {
    *contStack++ = cont;
    cont = &&contList;
    *tapeEls++ = tape - 1;
    *listCounter++ = sz | (size_t)tag << 32;
contList: {
  size_t * lc = listCounter - 1;
  size_t n = (*lc) & 0xFFFFFFFF;
  uint8_t tag = (*lc) >> 32;
  if (n != 0) {
    *lc = (*lc) - 1;
    if (tag > 12) goto error;
    goto *labels[tag];
  }
}
    listCounter--;
    cont = *--contStack;
    uint64_t * listStart = *--tapeEls;
    *listStart = *listStart | (0xFFFFFFFF & (tape - tapeStart));
    *tape++ = ((uint64_t)13 << 56) | (0xFFFFFFFF & (listStart - tapeStart));
  }
  goto *cont;
}
tagCompound: {
  if (contStack - contStackArr == MAX_DEPTH) goto error;
  *contStack++ = cont;
  cont = &&contCompound;
  *tapeEls++ = tape++;

contCompound:
  if (end == data) goto error;
  const uint8_t tag = *data++;

  if (tag != 0) {
    const size_t rem = end - data;
    if (rem < 2) goto error;
    const size_t sz = __builtin_bswap16(*((uint16_t *) data));
    if (rem - 2 < sz) goto error;
    data += sz + 2;
    if (tag > 12) goto error;
    goto *labels[tag];
  }

  cont = *--contStack;
  uint64_t * compStart = *--tapeEls;
  *compStart = ((uint64_t)10 << 56) | (0xFFFFFFFF & (tape - tapeStart));
  *tape++ = ((uint64_t)14 << 56) | (0xFFFFFFFF & (compStart - tapeStart));
  goto *cont;
}
error:
  return 0;

success:
  return tape - tapeStart;

}
