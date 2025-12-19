module Test.Common where

import Prelude

import Control.Monad.Except (class MonadError, ExceptT, runExceptT, throwError)
import Control.Monad.Writer (WriterT, runWriterT)
import Data.Either (Either(..))
import Data.Tuple.Nested ((/\))
import Effect.Exception (error)
import Effect.Exception as Exception
import Options.Applicative.Internal.Utils (unLines)
import Ripplat.Common (Error, Log)
import Text.Pretty (indent, unLines2)

type T m = ExceptT (Array Error) (WriterT (Array Log) m)

newSuccessTest :: forall m a. MonadError Exception.Error m => T m a -> m Unit
newSuccessTest m = do
  result /\ logs <- m
    # runExceptT
    # runWriterT
  case result of
    Left errs -> do
      throwError $ error $ unLines2
        [ "Not success"
        , unLines
            [ "Encountered errors:"
            , indent $ unLines2 $ map show errs
            ]
        , unLines
            [ "Logs:"
            , indent $ unLines $ map show logs
            ]
        ]
    Right _ -> pure unit
  pure unit
