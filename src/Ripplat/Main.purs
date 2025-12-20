module Ripplat.Main where

import Prelude

import Control.Monad.Except (class MonadError, throwError)
import Control.Monad.Logger (class MonadLogger)
import Control.Monad.RWS (RWSResult(..))
import Data.Foldable (null)
import Data.Tuple (Tuple(..))
import Effect.Class (class MonadEffect)
import Ripplat.Checking as Checking
import Ripplat.Common (Error, Log, toError)
import Ripplat.Grammr (Module)
import Ripplat.Interpretation (Gas)
import Ripplat.Interpretation as Interpretation
import Ripplat.Platform (Platform)
import Utility (runRWST')

main
  :: forall m r
   . MonadLogger Log m
  => MonadEffect m
  => MonadError (Array Error) m
  => { gas :: Gas
     , module_ :: Module
     , platform :: Platform m
     | r
     }
  -> m Unit
main args = do

  -- checking
  do
    RWSResult _ _ chErrs <- Checking.checkModule args.module_
      #
        ( _ `runRWST'`
            Tuple
              (Checking.newCtx args)
              (Checking.newEnv args)
        )

    unless (null chErrs) do
      throwError $ map (toError [ "check" ]) chErrs

  -- interpretation
  do
    RWSResult _ _ _ <- Interpretation.interpretModule args.module_
      #
        ( _ `runRWST'`
            Tuple
              (Interpretation.newCtx args)
              (Interpretation.newEnv args)

        )
    pure unit

  pure unit
