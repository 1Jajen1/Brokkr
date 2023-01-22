{-# LANGUAGE PatternSynonyms #-}
module Network.Packet.Server.Play.UseItemOn (
  Cursor(..)
, HeadInBlock(..)
) where

import Util.Binary
import Util.Linear.Vector
import Util.Linear.V3

pattern Cursor :: Float -> Float -> Float -> Cursor
pattern Cursor x y z = CursorI (V3_Float x y z)
{-# COMPLETE Cursor #-}

newtype Cursor = CursorI (V3 Float)
  deriving stock Show
  deriving newtype (Eq, VectorSpace Float)

instance ToBinary Cursor where
  put (Cursor x y z) = put x <> put y <> put z 

instance FromBinary Cursor where
  get = Cursor <$> get <*> get <*> get

data HeadInBlock = InBlock | NotInBlock
  deriving stock (Show, Eq)

instance FromBinary HeadInBlock where
  get = get @Bool >>= \case
    True  -> pure InBlock
    False -> pure NotInBlock
