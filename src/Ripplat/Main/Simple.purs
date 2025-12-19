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
import Ripplat.Grammr (Module)
import Ripplat.Interpretation as Interpretation
import Ripplat.Platform (Platform)
import Text.Pretty (indent, pretty)
import Utility (runRWST')

main
  :: forall m
   . MonadLogger Log m
  => MonadError (Array Error) m
  => { platform :: Platform m
     , module_ :: Module
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
          (Checking.newCtx { module_: args.module_ })
          (Checking.newEnv {})

    unless (null errs) do
      throwError $ map (toError [ "check" ]) errs

  -- interpretation
  interpretationEnv <- do
    RWSResult env _ errs <- Interpretation.interpretModule args.module_
      `runRWST'`
        Tuple
          ( Interpretation.newCtx
              { module_: args.module_
              , platform: args.platform
              }
          )
          (Interpretation.newEnv {})

    unless (null errs) do
      throwError $ map (toError [ "interpret" ]) errs

    log $ newLog [ "main" ] $ unLines
      [ "learned axioms:"
      , indent $ unLines $ map (indent <<< unLines <<< map \axiom -> pretty axiom.conc) $ env.axiomGroups
      ]

    pure env

  pure
    { interpretationEnv
    }

