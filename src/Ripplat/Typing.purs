module Ripplat.Typing where

import Prelude
import Ripplat.Grammr

import Control.Monad.Logger (class MonadLogger)
import Control.Monad.Reader (class MonadReader)
import Control.Monad.State (class MonadState)
import Data.Foldable (traverse_)
import Data.Lens ((.=))
import Data.List (List)
import Ripplat.Common (Log)
import Utility

type Ctx = {}

type Env =
  { tyDefs :: List TyDef
  , latDefs :: List LatDef
  , propDefs :: List PropDef
  }

newEnv :: {} -> Env
newEnv {} =
  { tyDefs: mempty
  , latDefs: mempty
  , propDefs: mempty
  }

typecheckModule
  :: forall m
   . MonadLogger Log m
  => MonadReader Ctx m
  => MonadState Env m
  => Module
  -> m Unit
typecheckModule (Module mdl) = do
  prop' @"tyDefs" .= mdl.tyDefs
  prop' @"latDefs" .= mdl.latDefs
  prop' @"propDefs" .= mdl.propDefs

  typecheckTyDef `traverse_` mdl.tyDefs
  typecheckLatDef `traverse_` mdl.latDefs
  typecheckPropDef `traverse_` mdl.propDefs
  typecheckRuleDef `traverse_` mdl.ruleDefs

  pure unit

typecheckTyDef
  :: forall m
   . MonadLogger Log m
  => MonadReader Ctx m
  => MonadState Env m
  => TyDef
  -> m Unit
typecheckTyDef = todo "typecheckTyDef"

typecheckLatDef
  :: forall m
   . MonadLogger Log m
  => MonadReader Ctx m
  => MonadState Env m
  => LatDef
  -> m Unit
typecheckLatDef = todo "typecheckLatDef"

typecheckPropDef
  :: forall m
   . MonadLogger Log m
  => MonadReader Ctx m
  => MonadState Env m
  => PropDef
  -> m Unit
typecheckPropDef = todo "typecheckPropDef"

typecheckRuleDef
  :: forall m
   . MonadLogger Log m
  => MonadReader Ctx m
  => MonadState Env m
  => RuleDef
  -> m Unit
typecheckRuleDef = todo "typecheckRuleDef"

