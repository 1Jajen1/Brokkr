# Brokkr-NBT

[![BSD-3-Clause license](https://img.shields.io/badge/license-BSD--3--Clause-blue.svg)](LICENSE)

Very fast NBT serialization library.

Correctly implements the NBT specification as presented by https://web.archive.org/web/20110723210920/http://www.minecraft.net/docs/NBT.txt, which includes correctly handling modified utf-8.

## Documentation

The goal of this library is serialization and deserialization into domain datatypes. It is possible to also obtain the intermediate NBT structure, but not many tools for working with this structure exist.

The library presents two approaches to parsing and encoding NBT with different strengths and weaknesses:
* `ByteString` ↔ `Brokkr.NBT.NBT` ↔ `a`
* `ByteString` ↔ `a` (Codec)

If possible use `Brokkr.NBT.Codec` to parse and encode your data. It generates by far the fastest parsers. This comes at a cost: The codec has to be defined using typed-template-haskell and the parser is generated at compile time. It is also not quite as flexible as the other approach.

If using `Brokkr.NBT.Codec` is not possible or too cumbersome, a more traditional interface which first parses into an intermediate NBT structure is offered in `Brokkr.NBT` and `Brokkr.NBT.Class`. It is slower than the codec method because requires allocation and sorting the keys of the compound for faster lookups in the future. The cost is not too prohibitive though, so long as NBT parsing is not a bottleneck, this method will perform just fine. And compared to other NBT parsers (ignoring codec generated ones) it is still the fastest implementation (that I know of).

Serialization is comparably fast for both approaches, but codecs still win by a small margin.

### Allocation and copies

NBT parsing avoids allocation and copies as much as possible, which is a major reason for its performance. Even with the intermediate structure only the structure is allocated. Strings and arrays are sliced directly from the input string without copying whenever possible. The methods involved will clearly document this behavior and offer clean ways to then copy the data to free the input.

Strings are always validated to be correctly encoded modified utf-8, but are not automatically transformed to a sane format (utf-8). The library does provide functions in `Brokkr.NBT.NBTString` which do convert and those *may* allocate, but will also clearly mention this fact. Some combinators are also offered which already perform this conversion.

Number arrays are also not transformed to host byte order and instead kept in their big-endian form. This is indicated by a newtype and performant options to transform into host byte order are available in `Brokkr.NBT.ByteOrder`. Again, combinators are also offered which already perform this conversion.

### Examples

TODO

## Parser and Builder choice

Brokkr uses `flatparse` and `mason` everywhere. This library is pretty much unusable without also using both these libraries.

## Contributing

Make sure the tests pass. Add one in case of a regression. And ensure the benchmarks don't regress.
