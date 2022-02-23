{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiWayIf #-}
module Effect.IO.File.Handle (
  runFile
) where

import Effectful
import Effect.IO.File.Effect
import System.IO (Handle, openBinaryFile, IOMode(..), hClose, hSeek, SeekMode(..))
import Effectful.Dispatch.Dynamic (interpret)
import qualified Data.Text as T
import Effect.IO.File.OpenFlags
import qualified Data.ByteString as BS

runFile :: IOE :> es => Eff (File Handle : es) a -> Eff es a
runFile = interpret $ \_ -> \case
  -- TODO This does not handle OpenCreate yet but it should!
  OpenAt (FilePath path) mode -> do
    let ioMode = if | mode `has` OpenReadOnly -> ReadMode
                    | mode `has` OpenWriteOnly -> WriteMode
                    | mode `has` OpenReadWrite -> ReadWriteMode 
                    | otherwise -> error "Unknown read mode"
    liftIO $ openBinaryFile (T.unpack path) ioMode
  ReadAt offset sz file -> liftIO $ do
    hSeek file AbsoluteSeek (fromIntegral offset)
    BS.hGet file sz
  Close file -> liftIO $ hClose file
