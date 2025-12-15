module Ripplat.Checking where

import Prelude
import Ripplat.Grammr
import Utility

import Control.Monad.Except (throwError, class MonadError)
import Control.Monad.Logger (class MonadLogger, log)
import Control.Monad.Reader (class MonadReader)
import Control.Monad.State (class MonadState, gets)
import Control.Monad.Writer (class MonadWriter, tell)
import Data.Foldable (length, traverse_)
import Data.Lens (view, (.=))
import Data.Lens.At (at)
import Data.List (List)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Tuple.Nested ((/\))
import Ripplat.Common (Error(..), Log, makeError, makeLog)
import Text.Pretty (pretty)

--------------------------------------------------------------------------------

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

newtype CheckError = CheckError
  { label :: String
  , source :: String
  , msg :: String
  }

makeCheckError label source msg = CheckError { label, source, msg }

derive instance Newtype CheckError _

--------------------------------------------------------------------------------

normalizeTy
  :: forall m
   . MonadLogger Log m
  => MonadError Error m
  => MonadReader Ctx m
  => MonadState Env m
  => MonadWriter (Array CheckError) m
  => WeirdTy
  -> m NormTy
normalizeTy (AppTy x ts) = do
  result <- gets $ view $ prop' @"tyDefs" <<< at x
  case result of
    Nothing -> throwError $ makeError [ "check" ] $ "Unknown reference to type of the name \"" <> unwrap x <> "\""
    Just (TyDef td) -> normalizeTy td.ty
normalizeTy UnitTy = pure UnitTy
normalizeTy BoolTy = pure BoolTy

normalizeLat
  :: forall m
   . MonadLogger Log m
  => MonadError Error m
  => MonadReader Ctx m
  => MonadState Env m
  => MonadWriter (Array CheckError) m
  => WeirdLat
  -> m NormLat
normalizeLat (AppLat x ls) = do
  result <- gets $ view $ prop' @"latDefs" <<< at x
  case result of
    Nothing -> throwError $ makeError [ "check" ] $ "Unknown reference to lattice of the name \"" <> unwrap x <> "\""
    Just (LatDef ld) -> normalizeLat ld.lat
normalizeLat UnitLat = pure UnitLat
normalizeLat BoolLat = pure BoolLat

--------------------------------------------------------------------------------

checkModule
  :: forall m
   . MonadLogger Log m
  => MonadError Error m
  => MonadReader Ctx m
  => MonadState Env m
  => MonadWriter (Array CheckError) m
  => Module
  -> m Unit
checkModule (Module mdl) = do
  log $ makeLog [ "check" ] $ "module " <> unwrap mdl.name

  prop' @"tyDefs" .= (mdl.tyDefs <#> (\it -> (unwrap it).name /\ it) # Map.fromFoldable)
  prop' @"latDefs" .= (mdl.latDefs <#> (\it -> (unwrap it).name /\ it) # Map.fromFoldable)
  prop' @"propDefs" .= (mdl.propDefs <#> (\it -> (unwrap it).name /\ it) # Map.fromFoldable)

  checkWeirdTyDef `traverse_` mdl.tyDefs
  checkWeirdLatDef `traverse_` mdl.latDefs
  checkPropDef `traverse_` mdl.propDefs
  checkRuleDef `traverse_` mdl.ruleDefs

  pure unit

--------------------------------------------------------------------------------

checkWeirdTyDef
  :: forall m
   . MonadLogger Log m
  => MonadReader Ctx m
  => MonadState Env m
  => MonadWriter (Array CheckError) m
  => MonadError Error m
  => TyDef
  -> m Unit
checkWeirdTyDef (TyDef td) = do
  log $ makeLog [ "check" ] $ "type " <> unwrap td.name
  checkWeirdTy td.ty

checkWeirdLatDef
  :: forall m
   . MonadLogger Log m
  => MonadReader Ctx m
  => MonadState Env m
  => MonadWriter (Array CheckError) m
  => MonadError Error m
  => LatDef
  -> m Unit
checkWeirdLatDef (LatDef ld) = do
  log $ makeLog [ "check" ] $ "lattice " <> unwrap ld.name
  checkWeirdLat ld.lat
  pure unit

checkPropDef
  :: forall m
   . MonadLogger Log m
  => MonadReader Ctx m
  => MonadState Env m
  => MonadWriter (Array CheckError) m
  => MonadError Error m
  => PropDef
  -> m Unit
checkPropDef (PropDef pd) = do
  log $ makeLog [ "check" ] $ "prop " <> unwrap pd.name
  checkWeirdLat `traverse_` pd.params

checkRuleDef
  :: forall m
   . MonadLogger Log m
  => MonadReader Ctx m
  => MonadState Env m
  => MonadWriter (Array CheckError) m
  => MonadError Error m
  => RuleDef
  -> m Unit
checkRuleDef (RuleDef rd) = do
  log $ makeLog [ "check" ] $ "rule (def) " <> unwrap (unwrap rd.rule).name
  checkRule rd.rule

checkRule
  :: forall m
   . MonadLogger Log m
  => MonadReader Ctx m
  => MonadState Env m
  => MonadWriter (Array CheckError) m
  => MonadError Error m
  => Rule
  -> m Unit
checkRule (Rule r) = do
  log $ makeLog [ "check" ] $ "rule " <> unwrap r.name
  checkProp `traverse_` r.hyps
  checkProp r.conc

checkWeirdTy
  :: forall m
   . MonadLogger Log m
  => MonadReader Ctx m
  => MonadState Env m
  => MonadWriter (Array CheckError) m
  => MonadError Error m
  => WeirdTy
  -> m Unit
checkWeirdTy t0@(AppTy x ts) = do
  result <- gets $ view $ prop' @"tyDefs" <<< at x
  case result of
    Nothing -> tell [ makeCheckError "type_ref" (pretty t0) $ "Unknown reference to type " <> pretty x <> "." ]
    Just td -> do
      let expectedArity = tyArity td
      let actualArity = length ts
      unless (expectedArity == actualArity) do
        tell [ makeCheckError "type_arity" (pretty t0) $ "The type family " <> pretty x <> " has arity " <> show expectedArity <> " but was only provided " <> show actualArity <> " arguments." ]
      checkWeirdTy `traverse_` ts
checkWeirdTy UnitTy = pure unit
checkWeirdTy BoolTy = pure unit

checkWeirdLat
  :: forall m
   . MonadLogger Log m
  => MonadReader Ctx m
  => MonadState Env m
  => MonadWriter (Array CheckError) m
  => MonadError Error m
  => WeirdLat
  -> m Unit
checkWeirdLat t0@(AppLat x ts) = do
  result <- gets $ view $ prop' @"latDefs" <<< at x
  case result of
    Nothing -> tell [ makeCheckError "lattice_ref" (pretty t0) $ "Unknown reference to lattice " <> pretty x <> "." ]
    Just ld -> do
      let expectedArity = latArity ld
      let actualArity = length ts
      unless (expectedArity == actualArity) do
        tell [ makeCheckError "lattice_arity" (pretty t0) $ "The lattice family " <> pretty x <> " has arity " <> show expectedArity <> " but was only provided " <> show actualArity <> " arguments." ]
      checkWeirdLat `traverse_` ts
checkWeirdLat UnitLat = pure unit
checkWeirdLat BoolLat = pure unit

-- TODO: this should work for any `id` type since i just dont do anything in that case?? or i should actually keep track of a mapping of what the inferred types are of each of the uses of a variable? 
checkProp
  :: forall m id
   . MonadLogger Log m
  => MonadReader Ctx m
  => MonadState Env m
  => MonadWriter (Array CheckError) m
  => MonadError Error m
  => Prop id
  -> m Unit
checkProp prop = todo "checkProp"
