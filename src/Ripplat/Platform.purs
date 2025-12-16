module Ripplat.Platform where

import Prelude

import Data.Newtype (class Newtype)

newtype Platform m = Platform
  {}

derive instance Newtype (Platform m) _

