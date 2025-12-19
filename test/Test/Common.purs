module Test.Common where

import Prelude

import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Writer (WriterT, runWriterT)
import Data.Array (concat)
import Data.Either (Either(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Effect.Exception (error)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (writeTextFile)
import Options.Applicative.Internal.Utils (unLines)
import Ripplat.Common (Error, Log)
import Test.Spec (Spec, it)
import Text.Pretty (indent, unLines2)

type T m = ExceptT (Array Error) (WriterT (Array Log) m)

newSuccessTest :: forall a. String -> T Aff a -> Spec Unit
newSuccessTest name m = it name do
  result /\ logs <- m
    # runExceptT
    # runWriterT
  let logFilePath = "log/" <> name <> ".log.txt"
  case result of
    Left errs -> do
      writeTextFile UTF8 logFilePath
        (unLines $ concat $ [ map show logs, map show errs ])
      throwError $ error $ unLines2
        [ "Not success"
        , unLines
            [ "Encountered errors:"
            , indent $ unLines2 $ map show errs
            ]
        ]
    Right _ -> do
      writeTextFile UTF8 logFilePath
        (unLines $ concat $ [ map show logs ])

