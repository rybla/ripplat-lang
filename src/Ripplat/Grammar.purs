module Ripplat.Grammr where

import Prelude

import Data.Eq.Generic (genericEq)
import Data.Foldable (length)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, unwrap)
import Data.Show.Generic (genericShow)
import Options.Applicative.Internal.Utils (unLines)
import Text.Pretty (class Pretty, commas, indent, pretty, unLines2)

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
  pretty (Module mdl) =
    unLines
      [ "module " <> unwrap mdl.name <> ":"
      , indent $ unLines2
          [ unLines2 $ map pretty mdl.tyDefs
          , unLines2 $ map pretty mdl.latDefs
          , unLines2 $ map pretty mdl.propDefs
          , unLines2 $ map pretty mdl.ruleDefs
          ]
      ]

--------------------------------------------------------------------------------

newtype PropDef = PropDef
  { name :: PropName
  , params :: Array WeirdLat
  }

derive instance Newtype PropDef _
derive newtype instance Show PropDef

instance Pretty PropDef where
  pretty (PropDef pd) =
    "prop " <> unwrap pd.name <> "(" <> (pd.params # map pretty # commas) <> ")"

newtype RuleDef = RuleDef
  { rule :: Rule
  }

derive instance Newtype RuleDef _
derive newtype instance Show RuleDef

instance Pretty RuleDef where
  pretty (RuleDef rd) = pretty rd.rule

newtype TyDef = TyDef
  { name :: TyName
  , params :: Array TyName
  , ty :: WeirdTy
  }

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

derive instance Newtype LatDef _
derive newtype instance Show LatDef

instance Pretty LatDef where
  pretty (LatDef ld) =
    "lattice " <> unwrap ld.name <> "(" <> (ld.params # map pretty # commas) <> ")"

--------------------------------------------------------------------------------

newtype Rule = Rule
  { name :: RuleName
  , hyps :: Array ColdProp
  , conc :: ColdProp
  }

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
  , args :: Array (Tm id)
  }

type ColdProp = Prop TrivialId
type HotProp = Prop HotId

instance Pretty id => Pretty (Prop id) where
  pretty (Prop p) = unwrap p.name <> "(" <> (p.args # map pretty # commas) <> ")"

derive instance Newtype (Prop id) _
derive newtype instance Show id => Show (Prop id)

data Ty name
  = AppTy name (Array (Ty name))
  | UnitTy
  | BoolTy

type WeirdTy = Ty TyName
type NormTy = Ty Void

derive instance Generic (Ty name) _

instance Show name => Show (Ty name) where
  show x = genericShow x

instance Eq name => Eq (Ty name) where
  eq x = genericEq x

instance Pretty name => Pretty (Ty name) where
  pretty (AppTy x ts) = pretty x <> "(" <> (ts # map pretty # commas) <> ")"
  pretty UnitTy = "Unit"
  pretty BoolTy = "Bool"

data Lat name
  = AppLat name (Array (Lat name))
  | UnitLat
  -- | False < True
  | BoolLat

type WeirdLat = Lat LatName
type NormLat = Lat Void

derive instance Generic (Lat name) _

instance Show name => Show (Lat name) where
  show x = genericShow x

instance Eq name => Eq (Lat name) where
  eq x = genericEq x

instance Pretty name => Pretty (Lat name) where
  pretty (AppLat x ts) = pretty x <> "(" <> (ts # map pretty # commas) <> ")"
  pretty UnitLat = "Unit"
  pretty BoolLat = "Bool"

data Tm var
  = VarTm (Var var)
  | UnitTm
  | BoolTm Boolean

type ColdTm = Tm TrivialId
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

type ColdVar = Var TrivialId
type HotVar = Var HotId

derive instance Newtype (Var var) _
derive newtype instance Show var => Show (Var var)
derive newtype instance Eq var => Eq (Var var)
derive newtype instance Ord var => Ord (Var var)

instance Pretty var => Pretty (Var var) where
  pretty (Var v) = unwrap v.name <> pretty v.id

data TrivialId = TrivialId

derive instance Generic TrivialId _

instance Show TrivialId where
  show x = genericShow x

instance Eq TrivialId where
  eq x = genericEq x

instance Pretty TrivialId where
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
derive newtype instance Pretty ModuleName
derive newtype instance Eq ModuleName
derive newtype instance Ord ModuleName

newtype PropName = PropName String

derive instance Newtype PropName _
derive newtype instance Show PropName
derive newtype instance Pretty PropName
derive newtype instance Eq PropName
derive newtype instance Ord PropName

newtype RuleName = RuleName String

derive instance Newtype RuleName _
derive newtype instance Show RuleName
derive newtype instance Pretty RuleName
derive newtype instance Eq RuleName
derive newtype instance Ord RuleName

newtype LatName = LatName String

derive instance Newtype LatName _
derive newtype instance Show LatName
derive newtype instance Pretty LatName
derive newtype instance Eq LatName
derive newtype instance Ord LatName

newtype TyName = TyName String

derive instance Newtype TyName _
derive newtype instance Show TyName
derive newtype instance Pretty TyName
derive newtype instance Eq TyName
derive newtype instance Ord TyName

newtype TmName = TmName String

derive instance Newtype TmName _
derive newtype instance Show TmName
derive newtype instance Pretty TmName
derive newtype instance Eq TmName
derive newtype instance Ord TmName

newtype VarName = VarName String

derive instance Newtype VarName _
derive newtype instance Show VarName
derive newtype instance Pretty VarName
derive newtype instance Eq VarName
derive newtype instance Ord VarName

--------------------------------------------------------------------------------

extractTy :: NormLat -> NormTy
extractTy (AppLat x _) = absurd x
extractTy UnitLat = UnitTy
extractTy BoolLat = BoolTy

latArity :: LatDef -> Int
latArity (LatDef td) = td.params # length

tyArity :: TyDef -> Int
tyArity (TyDef td) = td.params # length

propArity :: PropDef -> Int
propArity (PropDef pd) = pd.params # length

