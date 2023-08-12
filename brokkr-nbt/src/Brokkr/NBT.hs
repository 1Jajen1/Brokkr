module Brokkr.NBT (
  NBT
, Tag
, parseNBT
, putNBT
, module Brokkr.NBT.ByteOrder
, module Brokkr.NBT.NBTString
, module Brokkr.NBT.NBTError
) where

import Brokkr.NBT.Internal
import Brokkr.NBT.ByteOrder
import Brokkr.NBT.NBTString
import Brokkr.NBT.NBTError

{-
  TODO:
    - Add lens lib
    - Add optics lib
    - Add benchmarks in each package
      - lens/optics
    - utf-8 conversion
      - Add text dependency to write directly to text if we need to copy anyway
        Going through bytestring first is a perf penalty if we needed to copy
-}

-- Interface:
--
-- Manipulating nbt type:
-- Lenses are the nicest here, but move that to other package

{-

Things a high level api needs to do safely:
- Modified utf-8 handling
  - Matching should convert to modified utf8 and then match
  - When reading out convert to utf8
  => Ideas:
    - Store if the NBTString is already valid utf-8, which will often be the case
    - String literals are a bit annoying?
    - NBTString -> Text is annoying because I probably have to copy unless I have plain foreign ptrs
    - The conversion needs to be pure haskell code, which is fine as we only need it rarely

- Big endian handling
  - We byteswap all single element primitives. It is cheap enough there
  - We don't byteswap arrays eagerly because with an immutable interface
    we'd have to copy. We do have a really fast byteswapping implementation tho
  => Ideas:
    - Honestly just having IntBE on the api is fine, we can easily do
      toNative/unsafeToNative and clarify how and why it's unsafe
      - How is it unsafe/safe?
        It modifies the original input. So using unsafeToNative is safe as long as both the bytestring we
        initially parsed and the nbt we convert from is used at most once. So for example decoding and then
        using that exact same nbt object again to encode something is unsafe
        Is this fine? Probably. When parsing domain objects the input string and the nbt are usually thrown away anyway
        When modifying nbt lenses can can add a tiny bit more safety as we can swap back on write
-}

{-

Parsing into domain types:



-}

