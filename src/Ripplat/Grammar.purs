module Ripplat.Grammr where

import Prelude

import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.Newtype (class Newtype)
import Data.Ord.Generic (genericCompare)
import Data.Show.Generic (genericShow)
import Data.UUID (UUID)

--------------------------------------------------------------------------------

data Module = Module
  { name :: ModuleName
  , tyDefs :: List TyDef
  , propDefs :: List PropDef
  , ruleDefs :: List RuleDef
  }

derive instance Generic Module _

instance Show Module where
  show x = genericShow x

instance Eq Module where
  eq x = genericEq x

--------------------------------------------------------------------------------

data PropDef = PropDef
  { name :: PropName
  , params :: List Ty
  }

derive instance Generic PropDef _

instance Show PropDef where
  show x = genericShow x

instance Eq PropDef where
  eq x = genericEq x

data RuleDef = RuleDef
  { rule :: Rule
  }

derive instance Generic RuleDef _

instance Show RuleDef where
  show x = genericShow x

instance Eq RuleDef where
  eq x = genericEq x

data TyDef = TyDef
  { name :: String
  , ty :: Ty
  }

derive instance Generic TyDef _

instance Show TyDef where
  show x = genericShow x

instance Eq TyDef where
  eq x = genericEq x

data LatDef = LatDef
  { name :: LatName
  , lat :: Lat
  }

derive instance Generic LatDef _

instance Show LatDef where
  show x = genericShow x

instance Eq LatDef where
  eq x = genericEq x

--------------------------------------------------------------------------------

data Rule = Rule
  { name :: RuleName
  , hyps :: List RuleProp
  , conc :: RuleProp
  }

derive instance Generic Rule _

instance Show Rule where
  show x = genericShow x

instance Eq Rule where
  eq x = genericEq x

data Prop' id = Prop
  { name :: PropName
  , args :: List (Tm' id)
  }

type RuleProp = Prop' RuleId
type Prop = Prop' Id

derive instance Generic (Prop' id) _

instance Show id => Show (Prop' id) where
  show x = genericShow x

instance Eq id => Eq (Prop' id) where
  eq x = genericEq x

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

data Var' id = Var String id

type RuleVar = Var' RuleId
type Var = Var' Id

derive instance Generic (Var' id) _

instance Show id => Show (Var' id) where
  show x = genericShow x

instance Eq id => Eq (Var' id) where
  eq x = genericEq x

instance Ord id => Ord (Var' id) where
  compare x = genericCompare x

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

--------------------------------------------------------------------------------

fromLatToTy :: Lat -> Ty
fromLatToTy (RefLat x) = absurd x
fromLatToTy UnitLat = UnitTy
fromLatToTy BoolLat = BoolTy

