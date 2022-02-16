{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}
module Effect.IO.File.Effect (
  FilePath(..)
, File(..)
, openAt
, readAt
, close
) where

import Prelude hiding (FilePath)

import Effectful
import Data.Text
import Data.String
import Data.Kind
import Data.ByteString

import Effect.IO.File.OpenFlags
import Effectful.Dispatch.Dynamic

newtype FilePath = FilePath Text
  deriving newtype (Show, IsString)

--
type instance DispatchOf (File file) = 'Dynamic

-- | Low level dynamic file effect
data File (file :: Type) :: Effect where
  OpenAt :: FilePath -> OpenFlags -> File file m file
  ReadAt :: Int -> Int -> file -> File file m ByteString
  Close  :: file -> File file m ()

openAt :: File file :> es => FilePath -> OpenFlags -> Eff es file
openAt path flags = send $ OpenAt path flags

readAt :: File file :> es => Int -> Int -> file -> Eff es ByteString
readAt offset sz file = send $ ReadAt offset sz file

close :: File file :> es => file -> Eff es ()
close file = send $ Close file
