module Ripplat.Grammr where

import Prelude

import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)

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
  , hyps :: List Prop
  , conc :: Prop
  }

derive instance Generic Rule _

instance Show Rule where
  show x = genericShow x

instance Eq Rule where
  eq x = genericEq x

data Prop = Prop
  { name :: PropName
  , args :: List Tm
  }

derive instance Generic Prop _

instance Show Prop where
  show x = genericShow x

instance Eq Prop where
  eq x = genericEq x

data Ty = RefTy TyName

derive instance Generic Ty _

instance Show Ty where
  show x = genericShow x

instance Eq Ty where
  eq x = genericEq x

data Lat = RefLat LatName

derive instance Generic Lat _

instance Show Lat where
  show x = genericShow x

instance Eq Lat where
  eq x = genericEq x

data Tm = RefTm TmName

derive instance Generic Tm _

instance Show Tm where
  show x = genericShow x

instance Eq Tm where
  eq x = genericEq x

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

