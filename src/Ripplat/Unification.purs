module Ripplat.Unification where

import Data.Tuple.Nested
import Prelude
import Ripplat.Grammr

import Control.Monad.Error.Class (class MonadError, throwError)
import Control.Monad.State (class MonadState)
import Data.Lens ((%=))
import Data.Lens.Record (prop)
import Data.Map (Map)
import Data.Map as Map
import Type.Prelude (Proxy(..))

type Problem = Tm /\ Tm

type Env =
  { sigma :: Map Var Tm
  }

unify :: forall m. MonadState Env m => MonadError Problem m => Problem -> m Unit
unify (VarTm x1 /\ t2) = prop (Proxy @"sigma") %= Map.insert x1 t2
unify (t1 /\ VarTm x2) = prop (Proxy @"sigma") %= Map.insert x2 t1
unify (UnitTm /\ UnitTm) = pure unit
unify (BoolTm b1 /\ BoolTm b2) | b1 == b2 = pure unit
unify p = throwError p

