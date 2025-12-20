module Ripplat.Main.Simple where

import Prelude

import Control.Monad.Except (class MonadError, throwError)
import Control.Monad.Logger (class MonadLogger, log)
import Control.Monad.RWS (RWSResult(..))
import Data.Foldable (null)
import Data.Tuple (Tuple(..))
import Options.Applicative.Internal.Utils (unLines)
import Ripplat.Checking as Checking
import Ripplat.Common (Error, Log, newLog, toError)
import Ripplat.Grammr (Module, prettyConclusion)
import Ripplat.Interpretation (Gas)
import Ripplat.Interpretation as Interpretation
import Ripplat.Platform (Platform)
import Text.Pretty (indent, indentBullet)
import Utility (runRWST')

main
  :: forall m r
   . MonadLogger Log m
  => MonadError (Array Error) m
  => { platform :: Platform m
     , module_ :: Module
     , gas :: Gas
     | r
     }
  -> m
       { interpretationEnv :: Interpretation.Env
       }
main args = do
  log $ newLog [ "main" ] $ "executable: Simple"

  -- checking
  do
    RWSResult _ _ errs <- Checking.checkModule args.module_
      `runRWST'`
        Tuple
          (Checking.newCtx args)
          (Checking.newEnv args)

    unless (null errs) do
      throwError $ map (toError [ "check" ]) errs

  -- interpretation
  interpretationEnv <- do
    RWSResult env _ errs <- Interpretation.interpretModule args.module_
      `runRWST'`
        Tuple
          (Interpretation.newCtx args)
          (Interpretation.newEnv args)

    unless (null errs) do
      throwError $ map (toError [ "interpret" ]) errs

    log $ newLog [ "main" ] $ unLines
      [ "learned conclusions:"
      , indent $ unLines $ map (indentBullet <<< prettyConclusion) $ env.concs
      ]

    pure env

  pure
    { interpretationEnv
    }

