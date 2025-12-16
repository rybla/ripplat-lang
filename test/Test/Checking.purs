module Test.Checking where

import Prelude
import Ripplat.Checking
import Ripplat.Common
import Ripplat.Grammr
import Test.Spec

import Control.Monad.Except (class MonadError, throwError)
import Control.Monad.Reader (runReaderT)
import Control.Monad.State (evalStateT)
import Control.Monad.Writer (execWriterT)
import Data.Foldable (null)
import Data.Newtype (wrap)
import Effect.Class (class MonadEffect)
import Effect.Exception as Exception
import Test.Common as Common

spec :: Spec Unit
spec = describe "Checking" do
  it "ex1" $ newSuccessTest (Module { name: wrap "ex1", tyDefs: mempty, latDefs: mempty, propDefs: mempty, ruleDefs: mempty })
  it "ex2" $ newSuccessTest (Module { name: wrap "ex2", tyDefs: mempty, latDefs: mempty, propDefs: mempty, ruleDefs: mempty })
  it "ex3" $ newSuccessTest (Module { name: wrap "ex3", tyDefs: mempty, latDefs: mempty, propDefs: mempty, ruleDefs: mempty })
  it "ex4" $ newSuccessTest (Module { name: wrap "ex4", tyDefs: mempty, latDefs: mempty, propDefs: mempty, ruleDefs: mempty })
  it "ex5" $ newSuccessTest (Module { name: wrap "ex5", tyDefs: mempty, latDefs: mempty, propDefs: mempty, ruleDefs: mempty })

newSuccessTest
  :: forall m
   . MonadError Exception.Error m
  => Module
  -> m Unit
newSuccessTest mdl = Common.newSuccessTest do
  chErrs <- checkModule mdl
    # (_ `evalStateT` newEnv {})
    # (_ `runReaderT` newCtx {})
    # execWriterT

  unless (null chErrs) do
    throwError $ map (toError [ "checking" ]) chErrs

  pure unit
