{-# LANGUAGE PatternSynonyms #-}
module Effect.File (
  File
, openAt
, readAt
, close
, Effect.File.Effect.FilePath
, OpenFlags
, pattern OpenReadOnly
, pattern OpenWriteOnly
, pattern OpenReadWrite
, pattern OpenAppend
, pattern OpenCreate
, pattern OpenDirectIO
) where

import Effect.File.Effect
import Effect.File.OpenFlags
