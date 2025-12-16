module Ripplat.Main where

import Prelude
import Ripplat.Common
import Ripplat.Grammr

import Control.Monad.Except (class MonadError, throwError)
import Control.Monad.Logger (class MonadLogger)
import Control.Monad.Reader (runReaderT)
import Control.Monad.State (evalStateT)
import Control.Monad.Writer (execWriterT)
import Data.Foldable (null)
import Effect.Class (class MonadEffect)
import Ripplat.Checking as Checking
import Ripplat.Platform (Platform)

main
  :: forall m
   . MonadLogger Log m
  => MonadEffect m
  => MonadError (Array Error) m
  => Platform m
  -> Module
  -> m Unit
main pf mdl = do

  -- checking

  chErrs <- Checking.checkModule mdl
    # (_ `evalStateT` Checking.newEnv {})
    # (_ `runReaderT` Checking.newCtx {})
    # execWriterT

  unless (null chErrs) do
    throwError $ map (toError [ "checking" ]) chErrs

  -- interpretation

  pure unit
