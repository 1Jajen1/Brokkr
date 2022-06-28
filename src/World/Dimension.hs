{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
module World.Dimension (
  Dimension(..)
, HasDimension(..)
) where
import Optics

data Dimension = Overworld | Nether | TheEnd
  deriving stock (Eq, Enum, Show)

class HasDimension a where
  type Access a :: OpticKind
  dimension :: Optic' (Access a) NoIx a Dimension

