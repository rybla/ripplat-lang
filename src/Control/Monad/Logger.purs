module Control.Monad.Logger where

import Prelude

import Control.Monad.Writer (WriterT, tell)
import Effect.Class (class MonadEffect)
import Effect.Class.Console as Console

class Monad m <= MonadLogger log m where
  log :: log -> m Unit

instance Monad m => MonadLogger log (WriterT (Array log) m) where
  log = tell <<< pure

else instance (MonadEffect m, Show log) => MonadLogger log m where
  log = Console.logShow

