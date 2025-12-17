module Ripplat.Main where

import Prelude

import Ripplat.Common (Error, Log, toError)
import Ripplat.Grammr (Module)
import Control.Monad.Except (class MonadError, throwError)
import Control.Monad.Logger (class MonadLogger)
import Control.Monad.RWS (RWSResult(..), runRWST)
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
main _pf md = do

  -- checking

  RWSResult _ _ chErrs <- runRWST (Checking.checkModule md) (Checking.newCtx { module_: md }) (Checking.newEnv {})

  unless (null chErrs) do
    throwError $ map (toError [ "check" ]) chErrs

  -- interpretation

  -- TODO

  pure unit
