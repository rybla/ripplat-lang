module Ripplat.Common where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.String (joinWith)
import Options.Applicative.Internal.Utils (unLines)
import Text.Pretty (indent)

data Log = Log
  { path :: Array String
  , msg :: String
  }

derive instance Generic Log _

instance Show Log where
  show (Log l) = "[" <> (l.path # joinWith ".") <> "] " <> l.msg

newLog :: Array String -> String -> Log
newLog path msg = Log { path, msg }

data Error = Error
  { path :: ErrorPath
  , msg :: String
  }

derive instance Generic Error _

instance Show Error where
  show (Error l) = unLines
    [ "[" <> (l.path # joinWith ".") <> "]"
    , indent l.msg
    ]

type ErrorPath = Array String

newError :: Array String -> String -> Error
newError path msg = Error { path, msg }

class ToError a where
  toErrorMsg :: a -> String

toError :: forall a. ToError a => Array String -> a -> Error
toError path a = newError path (toErrorMsg a)
