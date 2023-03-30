{-# LANGUAGE DataKinds #-}
module Brokkr.BlockState.Internal.BlockEntry (
  BlockEntry(..)
, BlockState(..)
) where
import Deriving.Aeson
import qualified Data.Map.Strict as M
import Data.Text hiding (toLower)
import Data.Char (toLower)

-- JSON instances for BlockEntries
data LowerCase

instance StringModifier LowerCase where
  getStringModifier [] = []
  getStringModifier (x:xs) = toLower x : xs

data BlockEntry =
  BlockEntry {
    blockStates     :: [BlockState]
  , blockProperties :: Maybe (M.Map Text [Text])
  }
    deriving stock (Show, Generic)
    deriving FromJSON via CustomJSON '[
      OmitNothingFields
    , FieldLabelModifier (StripPrefix "block", LowerCase)
    ] BlockEntry

data BlockState =
  BlockState {
    stateId         :: Int
  , stateDefault    :: Maybe Bool
  , stateProperties :: Maybe (M.Map Text Text)
  }
    deriving stock (Show, Eq, Generic)
    deriving FromJSON via CustomJSON '[
      OmitNothingFields
    , FieldLabelModifier (StripPrefix "state", LowerCase)
    ] BlockState


