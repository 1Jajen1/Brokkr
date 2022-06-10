{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Util.Lift (
  Lift(..)
) where
import Control.Monad.IO.Class
import Control.Monad.Trans

newtype Lift t m a = Lift { unLift :: t m a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadTrans) 
