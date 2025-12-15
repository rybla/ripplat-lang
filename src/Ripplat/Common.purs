module Ripplat.Common where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.String (joinWith)

data Log = Log
  { path :: Array String
  , msg :: String
  }

derive instance Generic Log _

instance Show Log where
  show (Log l) = "[" <> (l.path # joinWith ".") <> "] " <> l.msg

makeLog path msg = Log { path, msg }

data Error = Error
  { path :: Array String
  , msg :: String
  }

derive instance Generic Error _

instance Show Error where
  show (Error l) = "[" <> (l.path # joinWith ".") <> "] " <> l.msg

makeError path msg = Error { path, msg }

