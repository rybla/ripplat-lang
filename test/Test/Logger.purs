module Test.Logger where

import Prelude

import Control.Monad.Error.Class (class MonadError, throwError)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Logger (class MonadLogger, log)
import Control.Monad.Writer (WriterT, runWriterT)
import Data.Either (Either(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Test.Spec (Spec, describe, it)

spec :: Spec Unit
spec = describe "Logger" do
  it "ex1" $ liftEffect do
    let
      m :: forall m. MonadError String m => MonadLogger String m => m Unit
      m = do
        log "hello"
        void $ throwError "error!"
        log "world"

      m1 :: WriterT (Array String) (ExceptT String Effect) Unit
      m1 = m

      m2 :: ExceptT String Effect Unit
      m2 = m

    do
      Console.log "[version 1]"
      runExceptT (runWriterT m1) >>= case _ of
        Left err -> Console.log $ "err = " <> show err
        Right (_ /\ logs) -> Console.log $ "logs = " <> show logs

    do
      Console.log "[version 2]"
      runExceptT m2 >>= case _ of
        Left err -> Console.log $ "err = " <> show err
        Right _ -> pure unit

    pure unit

