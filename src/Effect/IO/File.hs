{-# LANGUAGE PatternSynonyms #-}
module Effect.IO.File (
  File
, openAt
, readAt
, close
, Effect.IO.File.Effect.FilePath
, OpenFlags
, pattern OpenReadOnly
, pattern OpenWriteOnly
, pattern OpenReadWrite
, pattern OpenAppend
, pattern OpenCreate
, pattern OpenDirectIO
) where

import Effect.IO.File.Effect
import Effect.IO.File.OpenFlags
