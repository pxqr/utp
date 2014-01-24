module Network.UTP.Async
       (
       ) where

import Control.Concurrent
import Control.Exception


-- | For async recv.
type ARes = MVar (Either SomeException BL.ByteString)
--instance Monad ARes
--instance Applicative ARes
--instance Functor ARes
--instance Alternative ARes
--instance MonadPlus ARes