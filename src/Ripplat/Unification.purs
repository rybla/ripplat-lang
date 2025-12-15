module Ripplat.Unification where

import Prelude

import Data.Tuple.Nested (type (/\), (/\))
import Ripplat.Grammr (Tm, Tm'(..), Var)
import Control.Monad.Error.Class (class MonadError, throwError)
import Control.Monad.State (class MonadState)
import Data.Lens ((%=))
import Data.Map (Map)
import Data.Map as Map
import Utility (prop')

type Problem = Tm /\ Tm

type Env =
  { sigma :: Map Var Tm
  }

unify :: forall m. MonadState Env m => MonadError Problem m => Problem -> m Unit
unify (VarTm x1 /\ t2) = prop' @"sigma" %= Map.insert x1 t2
unify (t1 /\ VarTm x2) = prop' @"sigma" %= Map.insert x2 t1
unify (UnitTm /\ UnitTm) = pure unit
unify (BoolTm b1 /\ BoolTm b2) | b1 == b2 = pure unit
unify p = throwError p

