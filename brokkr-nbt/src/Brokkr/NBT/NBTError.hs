{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
module Brokkr.NBT.NBTError (
  NBTError(..)
, missingKey
, invalidType
) where

import Control.Exception (Exception)

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text qualified as T

-- TODO This whole thing is a mess and needs a rework

-- | Everything that can go wrong when parsing 'NBT'
data NBTError =
    InvalidTagType !Int               -- ^ The tag byte was completely invalid
  | InvalidStringEncoding !ByteString -- ^ The string is not valid modified utf-8
  | InvalidType !Text                 -- ^ Expected a different NBT type
  | MissingKey !Text                  -- ^ Missing keys when parsing from NBT
  -- TODO This really shouldn't be here
  | InvalidStringEnum !Text [Text]    -- ^ An enum encoded as string has an invalid value
  deriving stock Show
  deriving anyclass Exception

-- | Given a path and a number of missing keys, create an error message
missingKey :: [Text] -> [Text] -> NBTError
missingKey path keys = MissingKey $ "Missing keys [" <> T.dropEnd 1 (foldr (\y acc -> y <> "," <> acc) "" keys) <> "]" <> showPath path

-- | Given a path, an invalid type and an optional name create an error message
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
