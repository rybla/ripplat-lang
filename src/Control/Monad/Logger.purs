module Control.Monad.Logger where

import Prelude

import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (StateT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (WriterT, tell)
import Effect (Effect)
import Effect.Class.Console as Console

class Monad m <= MonadLogger log m where
  log :: log -> m Unit

instance Show log => MonadLogger log Effect where
  log = Console.logShow

-- monad transformers

instance MonadLogger log m => MonadLogger log (ReaderT r m) where
  log = lift <<< log

instance MonadLogger log m => MonadLogger log (StateT s m) where
  log = lift <<< log

instance MonadLogger log m => MonadLogger log (ExceptT e m) where
  log = lift <<< log

instance Monad m => MonadLogger log (WriterT (Array log) m) where
  log = tell <<< pure

else instance (MonadLogger log m, Monoid w) => MonadLogger log (WriterT w m) where
  log = lift <<< log

