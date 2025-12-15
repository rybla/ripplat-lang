module Ripplat.Engine where

import Prelude

import Control.Monad.Reader (class MonadReader)
import Control.Monad.State (class MonadState)
import Data.Map (Map)
import Ripplat.Grammr (Prop, PropName, RunProp)

type Ctx = {}

type Env =
  { props :: Map PropName RunProp
  }

run :: forall m. MonadReader Ctx m => MonadState Env m => m Unit
run = do
  pure unit

