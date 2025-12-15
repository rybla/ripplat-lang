module Ripplat.Grammr where

import Prelude

import Data.Eq.Generic (genericEq)
import Data.Foldable (length)
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.UUID (UUID)

--------------------------------------------------------------------------------

newtype Module = Module
  { name :: ModuleName
  , tyDefs :: List TyDef
  , latDefs :: List LatDef
  , propDefs :: List PropDef
  , ruleDefs :: List RuleDef
  }

derive instance Newtype Module _
derive newtype instance Show Module

--------------------------------------------------------------------------------

newtype PropDef = PropDef
  { name :: PropName
  , params :: List WeirdTy
  }

derive instance Newtype PropDef _
derive newtype instance Show PropDef

newtype RuleDef = RuleDef
  { rule :: Rule
  }

derive instance Newtype RuleDef _
derive newtype instance Show RuleDef

newtype TyDef = TyDef
  { name :: TyName
  , params :: List TyName
  , ty :: WeirdTy
  }

derive instance Newtype TyDef _
derive newtype instance Show TyDef

newtype LatDef = LatDef
  { name :: LatName
  , params :: List LatName
  , lat :: WeirdLat
  }

derive instance Newtype LatDef _
derive newtype instance Show LatDef

--------------------------------------------------------------------------------

newtype Rule = Rule
  { name :: RuleName
  , hyps :: List RuleProp
  , conc :: RuleProp
  }

derive instance Newtype Rule _
derive newtype instance Show Rule

newtype Prop' id = Prop
  { name :: PropName
  , args :: List (Tm' id)
  }

type RuleProp = Prop' RuleId
type Prop = Prop' Id

derive instance Newtype (Prop' id) _
derive newtype instance Show id => Show (Prop' id)

data Ty' name
  = RefTy name
  | UnitTy
  | BoolTy

type WeirdTy = Ty' TyName
type Ty = Ty' Void

derive instance Generic (Ty' name) _

instance Show name => Show (Ty' name) where
  show x = genericShow x

instance Eq name => Eq (Ty' name) where
  eq x = genericEq x

data Lat' name
  = RefLat name
  | UnitLat
  -- | False < True
  | BoolLat

type WeirdLat = Lat' LatName
type Lat = Lat' Void

derive instance Generic (Lat' name) _

instance Show name => Show (Lat' name) where
  show x = genericShow x

instance Eq name => Eq (Lat' name) where
  eq x = genericEq x

data Tm' id
  = VarTm (Var' id)
  | UnitTm
  | BoolTm Boolean

type RuleTm = Tm' RuleId
type Tm = Tm' Id

derive instance Generic (Tm' id) _

instance Show id => Show (Tm' id) where
  show x = genericShow x

instance Eq id => Eq (Tm' id) where
  eq x = genericEq x

newtype Var' id = Var { name :: VarName, id :: id }

type RuleVar = Var' RuleId
type Var = Var' Id

derive instance Newtype (Var' id) _
derive newtype instance Show id => Show (Var' id)
derive newtype instance Eq id => Eq (Var' id)
derive newtype instance Ord id => Ord (Var' id)

type RuleId = Unit
type Id = UUID

--------------------------------------------------------------------------------

newtype ModuleName = ModuleName String

derive instance Newtype ModuleName _
derive newtype instance Show ModuleName
derive newtype instance Eq ModuleName
derive newtype instance Ord ModuleName

newtype PropName = PropName String

derive instance Newtype PropName _
derive newtype instance Show PropName
derive newtype instance Eq PropName
derive newtype instance Ord PropName

newtype RuleName = RuleName String

derive instance Newtype RuleName _
derive newtype instance Show RuleName
derive newtype instance Eq RuleName
derive newtype instance Ord RuleName

newtype LatName = LatName String

derive instance Newtype LatName _
derive newtype instance Show LatName
derive newtype instance Eq LatName
derive newtype instance Ord LatName

newtype TyName = TyName String

derive instance Newtype TyName _
derive newtype instance Show TyName
derive newtype instance Eq TyName
derive newtype instance Ord TyName

newtype TmName = TmName String

derive instance Newtype TmName _
derive newtype instance Show TmName
derive newtype instance Eq TmName
derive newtype instance Ord TmName

newtype VarName = VarName String

derive instance Newtype VarName _
derive newtype instance Show VarName
derive newtype instance Eq VarName
derive newtype instance Ord VarName

--------------------------------------------------------------------------------

extractTy :: Lat -> Ty
extractTy (RefLat x) = absurd x
extractTy UnitLat = UnitTy
extractTy BoolLat = BoolTy

latArity :: LatDef -> Int
latArity (LatDef td) = td.params # length

tyArity :: TyDef -> Int
tyArity (TyDef td) = td.params # length

