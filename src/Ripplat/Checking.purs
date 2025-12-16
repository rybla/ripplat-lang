module Ripplat.Checking where

import Prelude
import Ripplat.Grammr
import Utility
import Ripplat.Common

import Control.Monad.Except (throwError, class MonadError)
import Control.Monad.Logger (class MonadLogger, log)
import Control.Monad.Reader (class MonadReader)
import Control.Monad.State (class MonadState, StateT, execStateT, gets)
import Control.Monad.Writer (class MonadWriter, tell)
import Control.Plus (empty)
import Data.Array as Array
import Data.Foldable (length, traverse_)
import Data.Lens (view, (.=))
import Data.Lens.At (at)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Traversable (traverse)
import Data.Tuple (uncurry)
import Data.Tuple.Nested ((/\))
import Options.Applicative.Internal.Utils (unLines)
import Text.Pretty (class Pretty, indent, pretty)

--------------------------------------------------------------------------------

type Ctx = {}

makeCtx :: {} -> Ctx
makeCtx _args =
  {}

type Env =
  { tyDefs :: Map TyName TyDef
  , latDefs :: Map LatName LatDef
  , propDefs :: Map PropName PropDef
  }

makeEnv :: {} -> Env
makeEnv _args =
  { tyDefs: empty
  , latDefs: empty
  , propDefs: empty
  }

newtype CheckError = CheckError
  { label :: String
  , source :: String
  , msg :: String
  }

makeCheckError label source msg = CheckError { label, source, msg }

derive instance Newtype CheckError _

instance ToError CheckError where
  toErrorMsg (CheckError ce) =
    unLines
      [ "checking error ()" <> ce.label <> ") at " <> ce.source <> ":"
      , indent ce.msg
      ]

--------------------------------------------------------------------------------

normalizeTy
  :: forall m
   . MonadLogger Log m
  => MonadError (Array Error) m
  => MonadReader Ctx m
  => MonadState Env m
  => MonadWriter (Array CheckError) m
  => WeirdTy
  -> m NormTy
normalizeTy (AppTy x _ts) = do
  result <- gets $ view $ prop' @"tyDefs" <<< at x
  case result of
    Nothing -> throwError $ pure $ newError [ "check" ] $ "Reference to unknown type of the name \"" <> unwrap x <> "\""
    Just (TyDef td) -> normalizeTy td.ty -- TODO: actually need to do substituion of args for params here
normalizeTy (UnitTy l) = pure $ UnitTy l
normalizeTy (BoolTy l) = pure $ BoolTy l

normalizeLatTy
  :: forall m
   . MonadLogger Log m
  => MonadError (Array Error) m
  => MonadReader Ctx m
  => MonadState Env m
  => MonadWriter (Array CheckError) m
  => WeirdLat
  -> m NormLat
normalizeLatTy (AppTy x _ts) = do
  result <- gets $ view $ prop' @"latDefs" <<< at x
  case result of
    Nothing -> throwError $ pure $ newError [ "check" ] $ "Reference to unknown type of the name \"" <> unwrap x <> "\""
    Just (LatDef ld) -> normalizeLatTy ld.lat -- TODO: actually need to do substituion of args for params here
normalizeLatTy (UnitTy l) = pure $ UnitTy l
normalizeLatTy (BoolTy l) = pure $ BoolTy l

--------------------------------------------------------------------------------

checkModule
  :: forall m
   . MonadLogger Log m
  => MonadError (Array Error) m
  => MonadReader Ctx m
  => MonadState Env m
  => MonadWriter (Array CheckError) m
  => Module
  -> m Unit
checkModule (Module mdl) = do
  log $ newLog [ "check" ] $ "module " <> unwrap mdl.name

  prop' @"tyDefs" .= (mdl.tyDefs <#> (\it -> (unwrap it).name /\ it) # Map.fromFoldable)
  prop' @"latDefs" .= (mdl.latDefs <#> (\it -> (unwrap it).name /\ it) # Map.fromFoldable)
  prop' @"propDefs" .= (mdl.propDefs <#> (\it -> (unwrap it).name /\ it) # Map.fromFoldable)

  checkWeirdTyDef `traverse_` mdl.tyDefs
  checkWeirdLatTyDef `traverse_` mdl.latDefs
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
  => MonadError (Array Error) m
  => TyDef
  -> m Unit
checkWeirdTyDef (TyDef td) = do
  log $ newLog [ "check" ] $ "type " <> unwrap td.name
  checkWeirdTy td.ty

checkWeirdLatTyDef
  :: forall m
   . MonadLogger Log m
  => MonadReader Ctx m
  => MonadState Env m
  => MonadWriter (Array CheckError) m
  => MonadError (Array Error) m
  => LatDef
  -> m Unit
checkWeirdLatTyDef (LatDef ld) = do
  log $ newLog [ "check" ] $ "lattice " <> unwrap ld.name
  checkWeirdLatTy ld.lat
  pure unit

checkPropDef
  :: forall m
   . MonadLogger Log m
  => MonadReader Ctx m
  => MonadState Env m
  => MonadWriter (Array CheckError) m
  => MonadError (Array Error) m
  => PropDef
  -> m Unit
checkPropDef (PropDef pd) = do
  log $ newLog [ "check" ] $ "prop " <> unwrap pd.name
  checkWeirdLatTy `traverse_` pd.params

checkRuleDef
  :: forall m
   . MonadLogger Log m
  => MonadReader Ctx m
  => MonadState Env m
  => MonadWriter (Array CheckError) m
  => MonadError (Array Error) m
  => RuleDef
  -> m Unit
checkRuleDef (RuleDef rd) = do
  log $ newLog [ "check" ] $ "rule (def) " <> unwrap (unwrap rd.rule).name
  checkRule rd.rule

checkRule
  :: forall m
   . MonadLogger Log m
  => MonadReader Ctx m
  => MonadState Env m
  => MonadWriter (Array CheckError) m
  => MonadError (Array Error) m
  => Rule
  -> m Unit
checkRule (Rule r) = do
  log $ newLog [ "check" ] $ "rule " <> unwrap r.name
  checkColdProp `traverse_` r.hyps
  checkColdProp r.conc

checkWeirdTy
  :: forall m
   . MonadLogger Log m
  => MonadReader Ctx m
  => MonadState Env m
  => MonadWriter (Array CheckError) m
  => MonadError (Array Error) m
  => WeirdTy
  -> m Unit
checkWeirdTy t0@(AppTy x ts) = do
  result <- gets $ view $ prop' @"tyDefs" <<< at x
  case result of
    Nothing -> tell [ makeCheckError "unknown_type" (pretty t0) $ "Reference to unknown type " <> pretty x <> "." ]
    Just td -> do
      let expectedArity = tyArity td
      let actualArity = length ts
      unless (expectedArity == actualArity) do
        tell [ makeCheckError "type_arity" (pretty t0) $ "The type family " <> pretty x <> " has arity " <> show expectedArity <> " but was only provided " <> show actualArity <> " arguments." ]
      checkWeirdTy `traverse_` ts
checkWeirdTy (UnitTy _) = pure unit
checkWeirdTy (BoolTy _) = pure unit

checkWeirdLatTy
  :: forall m
   . MonadLogger Log m
  => MonadReader Ctx m
  => MonadState Env m
  => MonadWriter (Array CheckError) m
  => MonadError (Array Error) m
  => WeirdLat
  -> m Unit
checkWeirdLatTy t0@(AppTy x ts) = do
  result <- gets $ view $ prop' @"latDefs" <<< at x
  case result of
    Nothing -> tell [ makeCheckError "unknown_lattice" (pretty t0) $ "Reference to unknown lattice " <> pretty x <> "." ]
    Just ld -> do
      let expectedArity = latArity ld
      let actualArity = length ts
      unless (expectedArity == actualArity) do
        tell [ makeCheckError "lattice_arity" (pretty t0) $ "The lattice family " <> pretty x <> " has arity " <> show expectedArity <> " but was only provided " <> show actualArity <> " arguments." ]
      checkWeirdLatTy `traverse_` ts
checkWeirdLatTy (UnitTy _) = pure unit
checkWeirdLatTy (BoolTy _) = pure unit

checkColdProp
  :: forall m
   . MonadLogger Log m
  => MonadReader Ctx m
  => MonadState Env m
  => MonadWriter (Array CheckError) m
  => MonadError (Array Error) m
  => ColdProp
  -> m Unit
checkColdProp p0@(Prop p) = do
  result <- gets $ view $ prop' @"propDefs" <<< at p.name
  case result of
    Nothing -> tell [ makeCheckError "unknown_prop" (pretty p0) $ "Reference to unknown proposition " <> pretty p.name <> "." ]
    Just (PropDef pd) -> do
      let expectedArity = PropDef pd # propArity
      let actualArity = p.args # length
      unless (expectedArity == actualArity) do
        tell [ makeCheckError "prop_arity" (pretty p0) $ "The proposition " <> pretty p.name <> " has arity " <> show expectedArity <> " but was only provided " <> show actualArity <> " arguments." ]
      doms <- normalizeLatTy `traverse` pd.params
      _sigma <- flip execStateT empty do
        uncurry checkColdTerm `traverse_` (doms `Array.zip` p.args)
      pure unit

checkColdTerm
  :: forall m lat
   . MonadLogger Log m
  => MonadReader Ctx m
  => MonadState Env m
  => MonadWriter (Array CheckError) m
  => MonadError (Array Error) m
  => Eq lat
  => Pretty lat
  => NormTy' lat
  -> ColdTm
  -> StateT (Map ColdVar (NormTy' lat)) m Unit
checkColdTerm s t0@(VarTm x) = do
  result <- gets $ view $ at x
  case result of
    Nothing -> tell [ makeCheckError "unknown_var" (pretty t0) $ "Reference to unknown variable " <> pretty x <> "." ]
    Just s' -> do
      unless (s == s') do
        tell [ makeCheckError "term_type" (pretty t0) $ "The variable " <> pretty t0 <> " was expected to have type " <> pretty s <> " but it was inferred to have type " <> pretty s' <> " in context." ]
checkColdTerm (UnitTy _) UnitTm = pure unit
checkColdTerm (BoolTy _) (BoolTm _) = pure unit
checkColdTerm s t = throwError $ pure $ newError [ "check" ] $ "The term " <> pretty t <> " does not satisfy the type " <> pretty s
