module Ripplat.Unification where

import Data.Tuple.Nested
import Prelude
import Ripplat.Grammr

import Control.Monad.Error.Class (class MonadError)
import Control.Monad.State (class MonadState)

type Problem = (Ty /\ Tm) /\ (Ty /\ Tm)

type Env = {}

data Error

unify :: forall m. MonadState Env m => MonadError Problem m => Problem -> m Unit
unify ((y1 /\ t1) /\ (y2 /\ t2)) = pure unit
