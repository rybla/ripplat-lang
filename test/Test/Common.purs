module Test.Common where

import Prelude
import Ripplat.Common

import Control.Monad.Except (class MonadError, ExceptT, runExceptT)
import Control.Monad.Writer (WriterT, runWriterT)
import Data.Either (Either(..))
import Data.Tuple.Nested ((/\))
import Effect.Exception as Exception
import Options.Applicative.Internal.Utils (unLines)
import Test.Spec.Assertions (fail)
import Text.Pretty (indent, unLines2)

newSuccessTest
  :: forall m a
   . MonadError Exception.Error m
  => ExceptT (Array Error) (WriterT (Array Log) m) a
  -> m Unit
newSuccessTest m = do
  result /\ logs <- m
    # runExceptT
    # runWriterT
  case result of
    Left errs -> do
      fail $ unLines2
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
