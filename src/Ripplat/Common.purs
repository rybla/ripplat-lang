module Ripplat.Common where

import Prelude

import Data.Generic.Rep (class Generic)

data Log = Log
  { label :: String
  , msg :: String
  }

derive instance Generic Log _

instance Show Log where
  show (Log l) = "[" <> l.label <> "] " <> l.msg
