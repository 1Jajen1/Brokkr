module Effect.World (
  WordEff
, RegionFileFolderPath(..)
) where

import qualified Effect.IO.File as File
import Effectful.Reader.Static
import Effectful
 
type WordEff w es =
  ( Reader (RegionFileFolderPath w) :> es
  )

newtype RegionFileFolderPath (w :: k) = RegionFileFolderPath File.FilePath
