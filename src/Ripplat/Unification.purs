module Ripplat.Unification where

import Prelude

import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.RWS (RWSResult(..), RWST)
import Control.Monad.Reader (ask, local)
import Control.Plus (empty)
import Data.Either.Nested (type (\/))
import Data.Lens ((%=), (%~))
import Data.List (List(..))
import Data.Map as Map
import Data.Tuple.Nested (type (/\), (/\))
import Ripplat.Grammr (HotProp, HotTm, HotVar, Prop(..), Tm(..), Substitution)
import Text.Pretty (class Pretty, commas, pretty, quoteCode)
import Utility (prop', runRWST')

--------------------------------------------------------------------------------

type T m = ExceptT UnificationError (RWST Ctx (List Assignment) Env m)

runT :: forall m a. Monad m => T m a -> m (Substitution /\ (UnificationError \/ a))
runT m = do
  RWSResult env res _ <- m
    # runExceptT
    # flip runRWST' (newCtx {} /\ newEnv {})
  pure $ env.sigma /\ res

type Problem = HotTm /\ HotTm

type Assignment = HotVar /\ HotTm

type Ctx =
  { path :: List Problem
  }

newCtx :: {} -> Ctx
newCtx {} = { path: mempty }

type Env =
  { sigma :: Substitution
  }

newEnv :: {} -> Env
newEnv {} = { sigma: empty }

data UnificationError
  = DifferentPropNames HotProp HotProp
  | UnsolvableProblem Ctx Problem

prettyProblem :: Problem -> String
prettyProblem (t1 /\ t2) = pretty t1 <> " ~ " <> pretty t2

instance Pretty UnificationError where
  pretty (DifferentPropNames p1 p2) = "Nominally incomparable propositions " <> quoteCode (pretty p1) <> " and " <> quoteCode (pretty p2)
  pretty (UnsolvableProblem ctx prob) = "Unsolvable unification problem " <> quoteCode (prettyProblem prob) <> " at path " <> (ctx.path # map prettyProblem # commas)

--------------------------------------------------------------------------------

unifyProps :: forall m. Monad m => HotProp -> HotProp -> T m Unit
unifyProps (Prop p1) (Prop p2) = do
  unless (p1.name == p2.name) $ throwError $ DifferentPropNames (Prop p1) (Prop p2)
  unify (p1.arg /\ p2.arg)

unify :: forall m. Monad m => Problem -> T m Unit
unify p =
  traceProblem p do
    unify' p

unify' :: forall m. Monad m => Problem -> T m Unit
unify' (VarTm x1 /\ t2) = prop' @"sigma" %= Map.insert x1 t2
unify' (t1 /\ VarTm x2) = prop' @"sigma" %= Map.insert x2 t1
unify' (UnitTm /\ UnitTm) = pure unit
unify' (BoolTm b1 /\ BoolTm b2) | b1 == b2 = pure unit
unify' p = do
  ctx <- ask
  throwError $ UnsolvableProblem ctx p

assignVar :: forall m. Monad m => HotVar -> HotTm -> T m Unit
assignVar x t = do
  prop' @"sigma" %= Map.insert x t

traceProblem :: forall m a. Monad m => Problem -> T m a -> T m a
traceProblem p m =
  local
    (prop' @"path" %~ Cons p)
    m

