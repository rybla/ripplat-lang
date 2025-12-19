module Ripplat.Checking where

import Prelude

import Control.Monad.Except (class MonadError)
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
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Tuple.Nested ((/\))
import Data.Unfoldable (none)
import Options.Applicative.Internal.Utils (unLines)
import Ripplat.Common (class ToError, Error, Log, newLog)
import Ripplat.Grammr (ColdProp, ColdTm, ColdVar, LatDef(..), LatName, Module(..), NormTy', Prop(..), PropDef(..), PropName, Rule, Rule'(..), RuleDef(..), Tm(..), Ty'(..), Ty''(..), TyDef(..), TyName, WeirdLat, WeirdTy, latArity, tyArity)
import Ripplat.Normalization (normalizeLatTy)
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

checkModule :: forall m. MonadLogger Log m => MonadError (Array Error) m => Module -> T m Unit
checkModule (Module md) = do
  log $ newLog [ "check" ] $ "module " <> unwrap md.name
  checkWeirdTyDef `traverse_` md.tyDefs
  checkWeirdLatTyDef `traverse_` md.latDefs
  checkPropDef `traverse_` md.propDefs
  checkRuleDef `traverse_` md.ruleDefs
  pure unit

--------------------------------------------------------------------------------

checkWeirdTyDef :: forall m. MonadLogger Log m => MonadError (Array Error) m => TyDef -> T m Unit
checkWeirdTyDef (TyDef td) = do
  log $ newLog [ "check" ] $ "type " <> unwrap td.name
  checkWeirdTy td.ty

checkWeirdLatTyDef :: forall m. MonadLogger Log m => MonadError (Array Error) m => LatDef -> T m Unit
checkWeirdLatTyDef (LatDef ld) = do
  log $ newLog [ "check" ] $ "lattice " <> unwrap ld.name
  checkWeirdLatTy ld.lat
  pure unit

checkPropDef :: forall m. MonadLogger Log m => MonadError (Array Error) m => PropDef -> T m Unit
checkPropDef (PropDef pd) = do
  log $ newLog [ "check" ] $ "prop " <> unwrap pd.name
  checkWeirdLatTy pd.param

checkRuleDef :: forall m. MonadLogger Log m => MonadError (Array Error) m => RuleDef -> T m Unit
checkRuleDef (RuleDef rd) = do
  log $ newLog [ "check" ] $ "rule (def) " <> unwrap (unwrap rd.rule).name
  checkRule rd.rule

checkRule :: forall m. MonadLogger Log m => MonadError (Array Error) m => Rule -> T m Unit
checkRule (Rule r) = do
  log $ newLog [ "check" ] $ "rule " <> unwrap r.name
  checkColdProp `traverse_` r.hyps
  checkColdProp r.conc

checkWeirdTy :: forall m. MonadError (Array Error) m => WeirdTy -> T m Unit
checkWeirdTy t0@(RefTy x ts) = do
  mb_td <- prop' @"tyDefs" <<< at x # view # asks >>= maybe (tell [ newCheckError "unknown_type" (pretty t0) $ "Reference to unknown type " <> quoteCode (pretty x) <> "." ] >>= const (pure none)) (pure <<< pure)
  case mb_td of
    Nothing -> pure unit
    Just td -> do
      let expectedArity = tyArity td
      let actualArity = length ts
      unless (expectedArity == actualArity) do
        tell [ newCheckError "type_arity" (pretty t0) $ "The type family " <> quoteCode (pretty x) <> " has arity " <> show expectedArity <> " but was only provided " <> show actualArity <> " arguments." ]
      checkWeirdTy `traverse_` ts
checkWeirdTy (Ty' _ UnitTy) = pure unit
checkWeirdTy (Ty' _ BoolTy) = pure unit

checkWeirdLatTy :: forall m. MonadError (Array Error) m => WeirdLat -> T m Unit
checkWeirdLatTy t0@(RefTy x ts) = do
  md_td <- prop' @"latDefs" <<< at x # view # asks >>= maybe (tell [ newCheckError "unknown_lattice" (pretty t0) $ "Reference to unknown lattice " <> quoteCode (pretty x) <> "." ] >>= const (pure none)) (pure <<< pure)
  case md_td of
    Nothing -> pure unit
    Just ld -> do
      let expectedArity = latArity ld
      let actualArity = length ts
      unless (expectedArity == actualArity) do
        tell [ newCheckError "lattice_arity" (pretty t0) $ "The lattice family " <> quoteCode (pretty x) <> " has arity " <> show expectedArity <> " but was only provided " <> show actualArity <> " arguments." ]
      checkWeirdLatTy `traverse_` ts
checkWeirdLatTy (Ty' _ UnitTy) = pure unit
checkWeirdLatTy (Ty' _ BoolTy) = pure unit

checkColdProp :: forall m. MonadError (Array Error) m => ColdProp -> T m Unit
checkColdProp p0@(Prop p) = do
  result <- prop' @"propDefs" <<< at p.name # view # asks >>= maybe (tell [ newCheckError "unknown_prop" (pretty p0) $ "Reference to unknown proposition " <> quoteCode (pretty p.name) <> "." ] >>= const (pure none)) (pure >>> pure)
  case result of
    Nothing -> pure unit
    Just (PropDef pd) -> do
      dom <- normalizeLatTy pd.param
      _sigma <- flip execStateT empty do
        checkColdTerm dom p.arg
      pure unit

checkColdTerm :: forall m lat. MonadError (Array Error) m => Eq lat => Pretty lat => NormTy' lat -> ColdTm -> StateT (Map ColdVar (NormTy' lat)) (T m) Unit
-- ensure that all uses of a variable are at the same lattice
checkColdTerm s t0@(VarTm x) = do
  result <- at x # view # gets
  case result of
    Nothing -> at x .= pure s
    Just s' -> unless (s == s') $ tell [ newCheckError "term_lattice" (pretty t0) $ "The variable " <> quoteCode (pretty t0) <> " was expected to have lattice " <> quoteCode (pretty s) <> " but it was inferred to have type " <> quoteCode (pretty s') <> " in context." ]
checkColdTerm (Ty' _ UnitTy) UnitTm = pure unit
checkColdTerm (Ty' _ BoolTy) (BoolTm _) = pure unit
checkColdTerm s t = tell [ newCheckError "term_lattice" (pretty t) $ "The term " <> quoteCode (pretty t) <> " does not satisfy the lattice " <> quoteCode (pretty s) ]

