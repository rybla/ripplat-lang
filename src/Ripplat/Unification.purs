module Ripplat.Unification where

import Prelude

import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (ExceptT)
import Control.Monad.RWS (RWST)
import Control.Monad.Reader (local)
import Data.Lens ((%=), (%~))
import Data.List (List(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Tuple.Nested (type (/\), (/\))
import Ripplat.Grammr (HotTm, Tm(..), HotVar)
import Utility (prop')

type Problem = HotTm /\ HotTm

type Ctx =
  { path :: List Problem
  }

type Env =
  { sigma :: Map HotVar HotTm
  }

type T m = ExceptT Problem (RWST Ctx (Array (HotVar /\ HotTm)) Env m)

assignVar :: forall m. Monad m => HotVar -> HotTm -> T m Unit
assignVar x t = do
  prop' @"sigma" %= Map.insert x t

traceProblem :: forall m a. Monad m => HotTm -> HotTm -> T m a -> T m a
traceProblem t1 t2 m =
  local
    (prop' @"path" %~ Cons (t1 /\ t2))
    m

unify :: forall m. Monad m => Problem -> T m Unit
unify (VarTm x1 /\ t2) = prop' @"sigma" %= Map.insert x1 t2
unify (t1 /\ VarTm x2) = prop' @"sigma" %= Map.insert x2 t1
unify (UnitTm /\ UnitTm) = pure unit
unify (BoolTm b1 /\ BoolTm b2) | b1 == b2 = pure unit
unify p = throwError p

