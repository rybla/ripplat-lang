module Ripplat.Engine where

import Prelude

import Control.Monad.Reader (class MonadReader)
import Control.Monad.State (class MonadState)
import Data.Map (Map)
import Ripplat.Grammr (PropName, Prop)

type Ctx = {}

type Env =
  { props :: Map PropName Prop
  }

run :: forall m. MonadReader Ctx m => MonadState Env m => m Unit
run = do
  pure unit

