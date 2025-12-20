module Ripplat.Grammr where

import Prelude

import Control.Monad.Reader (ReaderT, asks)
import Data.Eq.Generic (genericEq)
import Data.Foldable (length, null)
import Data.Generic.Rep (class Generic)
import Data.Lens (view)
import Data.Lens.At (at)
import Data.List (List)
import Data.Map (Map)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Show.Generic (genericShow)
import Data.Traversable (traverse)
import Options.Applicative.Internal.Utils (unLines)
import Text.Pretty (class Pretty, commas, indent, paren, pretty, unLines2)

--------------------------------------------------------------------------------

-- TODO: also have TmDef (with params)

newtype Module = Module
  { name :: ModuleName
  , tyDefs :: Array TyDef
  , latDefs :: Array LatDef
  , propDefs :: Array PropDef
  , ruleDefs :: Array RuleDef
  }

derive instance Newtype Module _
derive newtype instance Show Module

instance Pretty Module where
  pretty (Module md) =
    unLines
      [ "module " <> unwrap md.name <> ":"
      , indent $ unLines2
          [ unLines2 $ map pretty md.tyDefs
          , unLines2 $ map pretty md.latDefs
          , unLines2 $ map pretty md.propDefs
          , unLines2 $ map pretty md.ruleDefs
          ]
      ]

--------------------------------------------------------------------------------

newtype PropDef = PropDef
  { name :: PropName
  , param :: WeirdLat
  }

newPropDef :: PropName -> WeirdLat -> PropDef
newPropDef name param = PropDef { name, param }

derive instance Newtype PropDef _
derive newtype instance Show PropDef

instance Pretty PropDef where
  pretty (PropDef pd) =
    "prop " <> unwrap pd.name <> "(" <> (pd.param # pretty) <> ")"

newtype RuleDef = RuleDef
  { rule :: Rule
  }

newRuleDef :: Rule -> RuleDef
newRuleDef rule = RuleDef { rule }

derive instance Newtype RuleDef _
derive newtype instance Show RuleDef

instance Pretty RuleDef where
  pretty (RuleDef rd) = pretty rd.rule

newtype TyDef = TyDef
  { name :: TyName
  , params :: Array TyName
  , ty :: WeirdTy
  }

newTyDef :: TyName -> Array TyName -> WeirdTy -> TyDef
newTyDef name params ty = TyDef { name, params, ty }

derive instance Newtype TyDef _
derive newtype instance Show TyDef

instance Pretty TyDef where
  pretty (TyDef td) =
    "type " <> unwrap td.name <> "(" <> (td.params # map pretty # commas) <> ")"

newtype LatDef = LatDef
  { name :: LatName
  , params :: Array LatName
  , lat :: WeirdLat
  }

newLatDef :: LatName -> Array LatName -> WeirdLat -> LatDef
newLatDef name params lat = LatDef { name, params, lat }

derive instance Newtype LatDef _
derive newtype instance Show LatDef

instance Pretty LatDef where
  pretty (LatDef ld) =
    "lattice " <> unwrap ld.name <> "(" <> (ld.params # map pretty # commas) <> ")"

--------------------------------------------------------------------------------

newtype Rule' id = Rule
  { name :: RuleName
  , hyps :: List (Prop id)
  , conc :: Prop id
  }

newRule :: RuleName -> List ColdProp -> ColdProp -> Rule
newRule name hyps conc = Rule { name, hyps, conc }

type HotRule = Rule' HotId
type Rule = Rule' ColdId

derive instance Newtype (Rule' id) _
derive newtype instance Show id => Show (Rule' id)

instance Pretty Rule where
  pretty (Rule r) = unLines
    [ "rule " <> unwrap r.name <> ":"
    , indent $ if null r.hyps then "∅" else unLines $ map pretty $ r.hyps
    , indent "----"
    , indent $ pretty $ r.conc
    ]

newtype Prop id = Prop
  { name :: PropName
  , arg :: Tm id
  }

newProp :: forall id. PropName -> Tm id -> Prop id
newProp name arg = Prop { name, arg }

minProp :: forall id. PropName -> NormLat -> Prop id
minProp name l = Prop { name, arg: minTm l }

minTm :: forall id. NormLat -> Tm id
minTm (RefTy x _) = absurd x
minTm (Ty' CanonicalLat UnitTy) = UnitTm
minTm (Ty' CanonicalLat BoolTy) = BoolTm false

type ColdProp = Prop ColdId
type HotProp = Prop HotId

instance Pretty id => Pretty (Prop id) where
  pretty (Prop p) = unwrap p.name <> "(" <> (p.arg # pretty) <> ")"

derive instance Newtype (Prop id) _
derive newtype instance Show id => Show (Prop id)

data Ty' lat name
  = RefTy name (Array (Ty' lat name))
  | Ty' lat (Ty'' lat name)

data Ty'' :: forall k1 k2. k1 -> k2 -> Type
data Ty'' lat name
  = UnitTy
  | BoolTy

type WeirdTy' lat = Ty' lat Unit
type NormTy' lat = Ty' lat Void

type Ty = Ty' Unit
type WeirdTy = Ty TyName
type NormTy = Ty Void

derive instance Generic (Ty' lat name) _

instance (Show lat, Show name) => Show (Ty' lat name) where
  show x = genericShow x

instance (Eq lat, Eq name) => Eq (Ty' lat name) where
  eq x = genericEq x

instance (Pretty name, Pretty lat) => Pretty (Ty' lat name) where
  pretty (RefTy x ts) = pretty x <> "(" <> (ts # map pretty # commas) <> ")"
  pretty (Ty' l t) = pretty t <> "@" <> pretty l

derive instance Generic (Ty'' lat name) _

instance (Show lat, Show name) => Show (Ty'' lat name) where
  show x = genericShow x

instance (Eq lat, Eq name) => Eq (Ty'' lat name) where
  eq x = genericEq x

instance (Pretty name, Pretty lat) => Pretty (Ty'' lat name) where
  pretty UnitTy = "Unit"
  pretty BoolTy = "Bool"

type LatTy = Ty' Lat

type WeirdLat = LatTy LatName
type NormLat = LatTy Void

data Lat = CanonicalLat

derive instance Generic Lat _

instance Show Lat where
  show x = genericShow x

instance Eq Lat where
  eq x = genericEq x

instance Pretty Lat where
  pretty CanonicalLat = "Canonical"

data Tm var
  = VarTm (Var var)
  | UnitTm
  | BoolTm Boolean

type ColdTm = Tm ColdId
type HotTm = Tm HotId

derive instance Generic (Tm var) _

instance Show var => Show (Tm var) where
  show x = genericShow x

instance Eq var => Eq (Tm var) where
  eq x = genericEq x

instance Pretty var => Pretty (Tm var) where
  pretty (VarTm x) = pretty x
  pretty UnitTm = "unit"
  pretty (BoolTm b) = if b then "true" else "false"

newtype Var var = Var { name :: VarName, id :: var }

newVar :: forall var. VarName -> var -> Var var
newVar name id = Var { name, id }

newColdVar :: VarName -> ColdVar
newColdVar name = Var { name, id: top } :: ColdVar

type ColdVar = Var ColdId
type HotVar = Var HotId

derive instance Newtype (Var var) _
derive newtype instance Show var => Show (Var var)
derive newtype instance Eq var => Eq (Var var)
derive newtype instance Ord var => Ord (Var var)

instance Pretty var => Pretty (Var var) where
  pretty (Var v) = unwrap v.name <> pretty v.id

-- | Written rules are initialized as `Nothing`. Then derivative rules (i.e.
-- | lemmas and conclusions) will have the numberings that indicate which variables
-- | of the same name have been unified or not. Hot identifiers are cooled down
-- | after unification is completed so that identifiers don't keep increasing
-- | indefinitely when generating fresh identifiers. Cold identifiers are
-- | locally unique to a rule problem.
newtype ColdId = ColdId (Maybe Int)

derive instance Newtype ColdId _
derive newtype instance Show ColdId
derive newtype instance Eq ColdId
derive newtype instance Ord ColdId
derive newtype instance Bounded ColdId

newColdId :: ColdId
newColdId = ColdId Nothing

instance Pretty ColdId where
  pretty _ = ""

-- | Hot identifiers are in intermediate or immediate results from unification.
-- | Hot idenfiers are locally unique to a proposition unification problem.
newtype HotId = HotId Int

derive instance Newtype HotId _
derive newtype instance Show HotId
derive newtype instance Eq HotId
derive newtype instance Ord HotId

instance Pretty HotId where
  pretty (HotId n) = "@" <> show n

--------------------------------------------------------------------------------

newtype ModuleName = ModuleName String

derive instance Newtype ModuleName _
derive newtype instance Show ModuleName
derive newtype instance Eq ModuleName
derive newtype instance Ord ModuleName
instance Pretty ModuleName where
  pretty = unwrap

newtype PropName = PropName String

derive instance Newtype PropName _
derive newtype instance Show PropName
derive newtype instance Eq PropName
derive newtype instance Ord PropName
instance Pretty PropName where
  pretty = unwrap

newtype RuleName = RuleName String

derive instance Newtype RuleName _
derive newtype instance Show RuleName
derive newtype instance Eq RuleName
derive newtype instance Ord RuleName
instance Pretty RuleName where
  pretty = unwrap

newtype LatName = LatName String

derive instance Newtype LatName _
derive newtype instance Show LatName
derive newtype instance Eq LatName
derive newtype instance Ord LatName
instance Pretty LatName where
  pretty = unwrap

newtype TyName = TyName String

derive instance Newtype TyName _
derive newtype instance Show TyName
derive newtype instance Eq TyName
derive newtype instance Ord TyName
instance Pretty TyName where
  pretty = unwrap

newtype TmName = TmName String

derive instance Newtype TmName _
derive newtype instance Show TmName
derive newtype instance Eq TmName
derive newtype instance Ord TmName
instance Pretty TmName where
  pretty = unwrap

newtype VarName = VarName String

derive instance Newtype VarName _
derive newtype instance Show VarName
derive newtype instance Eq VarName
derive newtype instance Ord VarName
instance Pretty VarName where
  pretty = unwrap

--------------------------------------------------------------------------------

-- | An conclusion is a rule that has no hypotheses.
type Conclusion id =
  { name :: RuleName
  , prop :: Prop id
  }

type ColdConclusion = Conclusion ColdId
type HotConclusion = Conclusion HotId

prettyConclusion :: forall id. Pretty id => Conclusion id -> String
prettyConclusion conc = "conclusion " <> paren (unwrap conc.name) <> " : " <> pretty conc.prop

--------------------------------------------------------------------------------

extractTy :: NormLat -> NormTy
extractTy (RefTy x _) = absurd x
extractTy (Ty' _ s) = Ty' unit (extractTy'' s)

extractTy'' :: forall lat. Ty'' lat Void -> Ty'' Unit Void
extractTy'' UnitTy = UnitTy
extractTy'' BoolTy = BoolTy

latArity :: LatDef -> Int
latArity (LatDef td) = td.params # length

tyArity :: TyDef -> Int
tyArity (TyDef td) = td.params # length

--------------------------------------------------------------------------------

type Substitution = Map HotVar HotTm

type SubstitutionT (m :: Type -> Type) = ReaderT Substitution m

substituteRule :: forall m. Monad m => HotRule -> SubstitutionT m HotRule
substituteRule (Rule r) = do
  hyps <- substituteProp `traverse` r.hyps
  conc <- substituteProp r.conc
  pure $ Rule r { hyps = hyps, conc = conc }

substituteConclusion :: forall m. Monad m => HotConclusion -> SubstitutionT m HotConclusion
substituteConclusion conclusion = do
  prop <- substituteProp conclusion.prop
  pure { name: conclusion.name, prop }

substituteProp :: forall m. Monad m => HotProp -> SubstitutionT m HotProp
substituteProp (Prop p) = do
  arg <- substituteTm p.arg
  pure $ Prop { name: p.name, arg }

substituteTm :: forall m. Monad m => HotTm -> SubstitutionT m HotTm
substituteTm t0@(VarTm v) = asks (view (at v)) <#> fromMaybe t0
substituteTm UnitTm = pure UnitTm
substituteTm (BoolTm b) = pure $ BoolTm b

