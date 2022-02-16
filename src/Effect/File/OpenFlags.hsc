{-# LANGUAGE PatternSynonyms #-}
module Effect.File.OpenFlags (
  OpenFlags(..)
, pattern OpenReadOnly
, pattern OpenWriteOnly
, pattern OpenReadWrite
, pattern OpenAppend
, pattern OpenCreate
, pattern OpenDirectIO
, has
) where

import Data.Bits
import Data.Word

#define _GNU_SOURCE
#include <fcntl.h>

newtype OpenFlags = OpenFlags Word32
  deriving newtype Num

instance Semigroup OpenFlags where
  OpenFlags b1 <> OpenFlags b2 = OpenFlags $ b1 .|. b2

instance Monoid OpenFlags where
  mempty = OpenFlags 0

has :: OpenFlags -> OpenFlags -> Bool
has (OpenFlags flags) (OpenFlags toTest) = toTest == (flags .&. toTest)

pattern OpenReadOnly :: OpenFlags
pattern OpenReadOnly = OpenFlags #{const O_RDONLY}

pattern OpenWriteOnly :: OpenFlags
pattern OpenWriteOnly = OpenFlags #{const O_WRONLY}

pattern OpenReadWrite :: OpenFlags
pattern OpenReadWrite = OpenFlags #{const O_RDWR}

pattern OpenAppend :: OpenFlags
pattern OpenAppend = OpenFlags #{const O_APPEND}

pattern OpenCreate :: OpenFlags
pattern OpenCreate = OpenFlags #{const O_CREAT}

pattern OpenDirectIO :: OpenFlags
pattern OpenDirectIO = OpenFlags #{const O_DIRECT}
