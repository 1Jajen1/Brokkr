# Brokkr-VarInt

[![BSD-3-Clause license](https://img.shields.io/badge/license-BSD--3--Clause-blue.svg)](LICENSE)

Fast and easy to use library for dealing with Minecrafts variable-length encoding.

## Documentation

Contains two high level modules `Brokkr.VarInt` and `Brokkr.VarLong` which provide a newtype, a parser and a builder. Both these modules are documented and should be fairly straightforward to use.

Lower level methods are also exposed via `Brokkr.VarInt.Encode` and `Brokkr.VarInt.Decode`. These modules are also documented, but not as extensively.

## Parser and Builder choice

Brokkr uses `flatparse` and `mason` everywhere. You are not forced to use the two, as the lower level api is independent, however it is highly recommended.

## Contributing

Make sure the tests pass. Add one in case of a regression. And ensure the benchmarks don't regress.

## Credits

Part of the implementation is derived from:
* https://github.com/as-com/varint-simd A rust package for fast varint encoding and decoding
* Daniel Lemire, Nathan Kurz, Christoph Rupp - Stream VByte: Faster Byte-Oriented Integer Compression, Information Processing Letters 130, 2018: https://arxiv.org/abs/1709.08990
* Jeff Plaisance, Nathan Kurz, Daniel Lemire - Vectorized VByte Decoding, International Symposium on Web Algorithms, 2015: https://arxiv.org/abs/1503.07387
