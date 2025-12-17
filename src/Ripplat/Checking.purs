module Ripplat.Checking where

import Prelude

import Control.Monad.Except (throwError, class MonadError)
import Control.Monad.Logger (class MonadLogger, log)
import Control.Monad.RWS (RWST, asks)
import Control.Monad.State (StateT, execStateT, gets)
import Control.Monad.Writer (tell)
import Control.Plus (empty)
import Data.Foldable (length, traverse_)
import Data.Lens (view, (.=))
import Data.Lens.At (at)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Tuple.Nested ((/\))
import Options.Applicative.Internal.Utils (unLines)
import Ripplat.Common (class ToError, Error, Log, newError, newLog)
import Ripplat.Grammr (ColdProp, ColdTm, ColdVar, LatDef(..), LatName, Module(..), NormLat, NormTy, NormTy', Prop(..), PropDef(..), PropName, Rule(..), RuleDef(..), Tm(..), Ty'(..), TyDef(..), TyName, WeirdLat, WeirdTy, latArity, tyArity)
import Text.Pretty (class Pretty, indent, pretty, quoteCode)
import Utility (prop')

--------------------------------------------------------------------------------

type T = RWST Ctx (Array CheckError) Env

type Ctx = CtxK ()
type CtxK r =
  { tyDefs :: Map TyName TyDef
  , latDefs :: Map LatName LatDef
  , propDefs :: Map PropName PropDef
  | r
  }

newCtx
  :: forall r
   . { module_ :: Module
     | r
     }
  -> Ctx
newCtx args =
  let
    Module md = args.module_
  in
    { tyDefs: md.tyDefs <#> (\it -> (unwrap it).name /\ it) # Map.fromFoldable
    , latDefs: md.latDefs <#> (\it -> (unwrap it).name /\ it) # Map.fromFoldable
    , propDefs: md.propDefs <#> (\it -> (unwrap it).name /\ it) # Map.fromFoldable
    }

type Env =
  {
  }

newEnv :: forall r. { | r } -> Env
newEnv _args =
  {
  }

newtype CheckError = CheckError
  { label :: String
  , source :: String
  , msg :: String
  }

newCheckError :: String -> String -> String -> CheckError
newCheckError label source msg = CheckError { label, source, msg }

derive instance Newtype CheckError _

instance ToError CheckError where
  toErrorMsg (CheckError ce) =
    unLines
      [ ce.label <> " at " <> ce.source <> ":"
      , indent ce.msg
      ]

--------------------------------------------------------------------------------

normalizeTy
  :: forall m
   . MonadLogger Log m
  => MonadError (Array Error) m
  => WeirdTy
  -> T m NormTy
normalizeTy (AppTy x _ts) = do
  result <- asks $ view $ prop' @"tyDefs" <<< at x
  case result of
    Nothing -> throwError [ newError [ "check" ] $ "Reference to unknown type of the name \"" <> unwrap x <> "\"" ]
    Just (TyDef td) -> normalizeTy td.ty -- TODO: actually need to do substituion of args for params here
normalizeTy (UnitTy l) = pure $ UnitTy l
normalizeTy (BoolTy l) = pure $ BoolTy l

normalizeLatTy
  :: forall m
   . MonadLogger Log m
  => MonadError (Array Error) m
  => WeirdLat
  -> T m NormLat
normalizeLatTy (AppTy x _ts) = do
  result <- asks $ view $ prop' @"latDefs" <<< at x
  case result of
    Nothing -> throwError [ newError [ "check" ] $ "Reference to unknown type of the name \"" <> unwrap x <> "\"" ]
    Just (LatDef ld) -> normalizeLatTy ld.lat -- TODO: actually need to do substituion of args for params here
normalizeLatTy (UnitTy l) = pure $ UnitTy l
normalizeLatTy (BoolTy l) = pure $ BoolTy l

--------------------------------------------------------------------------------

checkModule
  :: forall m
   . MonadLogger Log m
  => MonadError (Array Error) m
  => Module
  -> T m Unit
checkModule (Module md) = do
  log $ newLog [ "check" ] $ "module " <> unwrap md.name
  checkWeirdTyDef `traverse_` md.tyDefs
  checkWeirdLatTyDef `traverse_` md.latDefs
  checkPropDef `traverse_` md.propDefs
  checkRuleDef `traverse_` md.ruleDefs
  pure unit

--------------------------------------------------------------------------------

checkWeirdTyDef
  :: forall m
   . MonadLogger Log m
  => MonadError (Array Error) m
  => TyDef
  -> T m Unit
checkWeirdTyDef (TyDef td) = do
  log $ newLog [ "check" ] $ "type " <> unwrap td.name
  checkWeirdTy td.ty

checkWeirdLatTyDef
  :: forall m
   . MonadLogger Log m
  => MonadError (Array Error) m
  => LatDef
  -> T m Unit
checkWeirdLatTyDef (LatDef ld) = do
  log $ newLog [ "check" ] $ "lattice " <> unwrap ld.name
  checkWeirdLatTy ld.lat
  pure unit

checkPropDef
  :: forall m
   . MonadLogger Log m
  => MonadError (Array Error) m
  => PropDef
  -> T m Unit
checkPropDef (PropDef pd) = do
  log $ newLog [ "check" ] $ "prop " <> unwrap pd.name
  checkWeirdLatTy pd.param

checkRuleDef
  :: forall m
   . MonadLogger Log m
  => MonadError (Array Error) m
  => RuleDef
  -> T m Unit
checkRuleDef (RuleDef rd) = do
  log $ newLog [ "check" ] $ "rule (def) " <> unwrap (unwrap rd.rule).name
  checkRule rd.rule

checkRule
  :: forall m
   . MonadLogger Log m
  => MonadError (Array Error) m
  => Rule
  -> T m Unit
checkRule (Rule r) = do
  log $ newLog [ "check" ] $ "rule " <> unwrap r.name
  checkColdProp `traverse_` r.hyps
  checkColdProp r.conc

checkWeirdTy
  :: forall m
   . MonadLogger Log m
  => MonadError (Array Error) m
  => WeirdTy
  -> T m Unit
checkWeirdTy t0@(AppTy x ts) = do
  result <- asks $ view $ prop' @"tyDefs" <<< at x
  case result of
    Nothing -> tell [ newCheckError "unknown_type" (pretty t0) $ "Reference to unknown type " <> quoteCode (pretty x) <> "." ]
    Just td -> do
      let expectedArity = tyArity td
      let actualArity = length ts
      unless (expectedArity == actualArity) do
        tell [ newCheckError "type_arity" (pretty t0) $ "The type family " <> quoteCode (pretty x) <> " has arity " <> show expectedArity <> " but was only provided " <> show actualArity <> " arguments." ]
      checkWeirdTy `traverse_` ts
checkWeirdTy (UnitTy _) = pure unit
checkWeirdTy (BoolTy _) = pure unit

checkWeirdLatTy
  :: forall m
   . MonadLogger Log m
  => MonadError (Array Error) m
  => WeirdLat
  -> T m Unit
checkWeirdLatTy t0@(AppTy x ts) = do
  result <- asks $ view $ prop' @"latDefs" <<< at x
  case result of
    Nothing -> tell [ newCheckError "unknown_lattice" (pretty t0) $ "Reference to unknown lattice " <> quoteCode (pretty x) <> "." ]
    Just ld -> do
      let expectedArity = latArity ld
      let actualArity = length ts
      unless (expectedArity == actualArity) do
        tell [ newCheckError "lattice_arity" (pretty t0) $ "The lattice family " <> quoteCode (pretty x) <> " has arity " <> show expectedArity <> " but was only provided " <> show actualArity <> " arguments." ]
      checkWeirdLatTy `traverse_` ts
checkWeirdLatTy (UnitTy _) = pure unit
checkWeirdLatTy (BoolTy _) = pure unit

checkColdProp
  :: forall m
   . MonadLogger Log m
  => MonadError (Array Error) m
  => ColdProp
  -> T m Unit
checkColdProp p0@(Prop p) = do
  result <- asks $ view $ prop' @"propDefs" <<< at p.name
  case result of
    Nothing -> tell [ newCheckError "unknown_prop" (pretty p0) $ "Reference to unknown proposition " <> quoteCode (pretty p.name) <> "." ]
    Just (PropDef pd) -> do
      dom <- normalizeLatTy pd.param
      _sigma <- flip execStateT empty do
        checkColdTerm dom p.arg
      pure unit

checkColdTerm
  :: forall m lat
   . MonadLogger Log m
  => MonadError (Array Error) m
  => Eq lat
  => Pretty lat
  => NormTy' lat
  -> ColdTm
  -> StateT (Map ColdVar (NormTy' lat)) (T m) Unit
-- ensure that all uses of a variable are at the same lattice
checkColdTerm s t0@(VarTm x) = do
  result <- gets $ view $ at x
  case result of
    Nothing -> at x .= pure s
    Just s' -> do
      unless (s == s') do
        tell [ newCheckError "term_lattice" (pretty t0) $ "The variable " <> quoteCode (pretty t0) <> " was expected to have lattice " <> quoteCode (pretty s) <> " but it was inferred to have type " <> quoteCode (pretty s') <> " in context." ]
checkColdTerm (UnitTy _) UnitTm = pure unit
checkColdTerm (BoolTy _) (BoolTm _) = pure unit
checkColdTerm s t = tell [ newCheckError "term_lattice" (pretty t) $ "The term " <> quoteCode (pretty t) <> " does not satisfy the lattice " <> quoteCode (pretty s) ]
