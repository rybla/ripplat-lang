module Ripplat.Platform where

import Prelude

import Data.Newtype (class Newtype)

newtype Platform (m :: Type -> Type) = Platform
  { placeholder :: Unit }

derive instance Newtype (Platform m) _

mockPlatform :: forall m. Monad m => Platform m
mockPlatform = Platform
  { placeholder: unit
  }

