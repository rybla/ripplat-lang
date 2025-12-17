module Ripplat.Grammr where

import Prelude

import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.Eq.Generic (genericEq)
import Data.Foldable (length)
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Show.Generic (genericShow)
import Options.Applicative.Internal.Utils (unLines)
import Text.Pretty (class Pretty, commas, indent, pretty, unLines2)
import Utility (todo, todoK)

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

newtype Rule = Rule
  { name :: RuleName
  , hyps :: List ColdProp
  , conc :: ColdProp
  }

newRule :: RuleName -> List ColdProp -> ColdProp -> Rule
newRule name hyps conc = Rule { name, hyps, conc }

derive instance Newtype Rule _
derive newtype instance Show Rule

instance Pretty Rule where
  pretty (Rule r) = unLines
    [ "rule " <> unwrap r.name <> ":"
    , indent $ unLines $ map pretty $ r.hyps
    , indent "----"
    , indent $ pretty $ r.conc
    ]

newtype Prop id = Prop
  { name :: PropName
  , arg :: Tm id
  }

newProp :: forall id. PropName -> Tm id -> Prop id
newProp name arg = Prop { name, arg }

type ColdProp = Prop ColdId
type HotProp = Prop HotId

instance Pretty id => Pretty (Prop id) where
  pretty (Prop p) = unwrap p.name <> "(" <> (p.arg # pretty) <> ")"

derive instance Newtype (Prop id) _
derive newtype instance Show id => Show (Prop id)

data Ty' lat name
  = AppTy name (Array (Ty' lat name))
  | UnitTy lat
  | BoolTy lat

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
  pretty (AppTy x ts) = pretty x <> "(" <> (ts # map pretty # commas) <> ")"
  pretty (UnitTy l) = "Unit@" <> pretty l
  pretty (BoolTy l) = "Bool@" <> pretty l

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

-- TODO: actually, could hold a `Maybe Int` which is initialized to `Nothing`, so that when converting from HotProp to ColdProp we dont accidentally equate things by name if they've been substituted differently

-- | Written rules are initialized as `Nothing`. Then derivative rules (i.e.
-- | lemmas and axioms) will have the numberings that indicate which variables
-- | of the same name have been unified or not.
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

class LatOrUnit lat where
  fromLatOrUnit :: forall f. Functor f => f lat -> f Lat \/ f Unit

instance LatOrUnit Lat where
  fromLatOrUnit = Left

instance LatOrUnit Unit where
  fromLatOrUnit = Right

--------------------------------------------------------------------------------

extractTy :: NormLat -> NormTy
extractTy (AppTy x _) = absurd x
extractTy (UnitTy _) = UnitTy unit
extractTy (BoolTy _) = BoolTy unit

latArity :: LatDef -> Int
latArity (LatDef td) = td.params # length

tyArity :: TyDef -> Int
tyArity (TyDef td) = td.params # length

--------------------------------------------------------------------------------

type Substitution = Map HotVar HotTm

substituteProp :: Substitution -> HotProp -> HotProp
substituteProp = todoK "substituteProp"
