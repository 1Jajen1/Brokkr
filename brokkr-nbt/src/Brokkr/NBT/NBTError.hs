{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Brokkr.NBT.NBTError (
  NBTError(..)
, missingKey
, invalidType
) where

import Data.ByteString (ByteString)
import Data.Text (Text)

data NBTError =
    InvalidTagType !Int
  | InvalidStringEncoding !ByteString
  | InvalidType !Text
  | MissingKey !Text
  deriving stock Show

missingKey :: [Text] -> Text -> NBTError
missingKey path key = MissingKey $ "Missing key " <> key <> "." <> showPath path

invalidType :: [Text] -> Text -> Maybe Text -> NBTError
invalidType path ty Nothing = InvalidType $ "Invalid type. Expected " <> ty <> "." <> showPath path
invalidType path ty (Just name) = InvalidType $ "Invalid type for " <> name <> ". Expected " <> ty <> "." <> showPath path

showPath :: [Text] -> Text
showPath (Reverse []) = " At path <root>"
showPath (Reverse (x:xs)) = " At path " <> x <> foldr (\y acc -> "." <> y <> acc) "" xs

-- Honestly this only exists to make ghc shut up
--  on a warning on incomplete patterns
pattern Reverse :: [a] -> [a]
pattern Reverse xs <- (reverse -> xs)
  where Reverse xs = reverse xs
{-# COMPLETE Reverse #-}
