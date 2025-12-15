module Ripplat.Typing where

import Prelude
import Ripplat.Grammr
import Utility

import Control.Monad.Except (throwError, class MonadError)
import Control.Monad.Logger (class MonadLogger, log)
import Control.Monad.Reader (class MonadReader)
import Control.Monad.State (class MonadState, gets)
import Data.Foldable (traverse_)
import Data.Lens (view, (.=))
import Data.Lens.At (at)
import Data.List (List)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Tuple.Nested ((/\))
import Ripplat.Common (Error(..), Log, makeError, makeLog)

type Ctx = {}

type Env =
  { tyDefs :: Map TyName TyDef
  , latDefs :: Map LatName LatDef
  , propDefs :: Map PropName PropDef
  }

makeEnv :: {} -> Env
makeEnv {} =
  { tyDefs: Map.empty
  , latDefs: Map.empty
  , propDefs: Map.empty
  }

normalizeTy
  :: forall m
   . MonadLogger Log m
  => MonadError Error m
  => MonadReader Ctx m
  => MonadState Env m
  => WeirdTy
  -> m NormTy
normalizeTy (RefTy x) = gets (view (prop' @"tyDefs" <<< at x)) >>= case _ of
  Nothing -> throwError $ makeError [ "typecheck" ] $ "Unknown reference to type of the name \"" <> unwrap x <> "\""
  Just (TyDef td) -> normalizeTy td.ty
normalizeTy UnitTy = pure UnitTy
normalizeTy BoolTy = pure BoolTy

normalizeLat
  :: forall m
   . MonadLogger Log m
  => MonadError Error m
  => MonadReader Ctx m
  => MonadState Env m
  => WeirdLat
  -> m NormLat
normalizeLat (RefLat x) = gets (view (prop' @"latDefs" <<< at x)) >>= case _ of
  Nothing -> throwError $ makeError [ "typecheck" ] $ "Unknown reference to lattice of the name \"" <> unwrap x <> "\""
  Just (LatDef ld) -> normalizeLat ld.lat
normalizeLat UnitLat = pure UnitLat
normalizeLat BoolLat = pure BoolLat

typecheckModule
  :: forall m
   . MonadLogger Log m
  => MonadError Error m
  => MonadReader Ctx m
  => MonadState Env m
  => Module
  -> m Unit
typecheckModule (Module mdl) = do
  prop' @"tyDefs" .= (mdl.tyDefs <#> (\it -> (unwrap it).name /\ it) # Map.fromFoldable)
  prop' @"latDefs" .= (mdl.latDefs <#> (\it -> (unwrap it).name /\ it) # Map.fromFoldable)
  prop' @"propDefs" .= (mdl.propDefs <#> (\it -> (unwrap it).name /\ it) # Map.fromFoldable)

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
typecheckTyDef (TyDef td) = do
  log $ makeLog [ "typecheck" ] $ unwrap td.name
  pure unit

typecheckLatDef
  :: forall m
   . MonadLogger Log m
  => MonadReader Ctx m
  => MonadState Env m
  => LatDef
  -> m Unit
typecheckLatDef (LatDef ld) = do
  log $ makeLog [ "typecheck" ] $ unwrap ld.name
  todo "typecheckLatDef"

typecheckPropDef
  :: forall m
   . MonadLogger Log m
  => MonadReader Ctx m
  => MonadState Env m
  => PropDef
  -> m Unit
typecheckPropDef (PropDef pd) = do
  log $ makeLog [ "typecheck" ] $ unwrap pd.name
  todo "typecheckPropDef"

typecheckRuleDef
  :: forall m
   . MonadLogger Log m
  => MonadReader Ctx m
  => MonadState Env m
  => RuleDef
  -> m Unit
typecheckRuleDef (RuleDef rd) = do
  log $ makeLog [ "typecheck" ] $ unwrap (rd.rule # unwrap).name
  todo "typecheckRuleDef"

