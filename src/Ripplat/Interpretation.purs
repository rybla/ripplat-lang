module Ripplat.Interpretation where

import Prelude
import Ripplat.Common
import Ripplat.Grammr
import Ripplat.Platform
import Utility

import Control.Monad.Error.Class (class MonadError)
import Control.Monad.Logger (class MonadLogger)
import Control.Monad.Reader (class MonadReader, ReaderT)
import Control.Monad.State (class MonadState, StateT)
import Control.Monad.Writer (class MonadWriter, WriterT)
import Data.Newtype (class Newtype)
import Options.Applicative.Internal.Utils (unLines)
import Text.Pretty (indent)

--------------------------------------------------------------------------------

type Ctx m =
  { platform :: Platform m
  }

newCtx
  :: forall m
   . Monad m
  => { platform :: Platform m }
  -> Ctx m
newCtx args =
  { platform: args.platform
  }

type Env = {}

newEnv :: {} -> Env
newEnv _args =
  {}

newtype InterpretError = InterpretError
  { label :: String
  , source :: String
  , msg :: String
  }

newInterpretError label source msg = InterpretError { label, source, msg }

derive instance Newtype InterpretError _

instance ToError InterpretError where
  toErrorMsg (InterpretError ce) =
    unLines
      [ ce.label <> " at " <> ce.source <> ":"
      , indent ce.msg
      ]

--------------------------------------------------------------------------------

-- type T m = ReaderT (Ctx m) (WriterT (Array InterpretError) (StateT Env m))

-- interpretModule
--   :: forall m
--    . MonadLogger Log m
--   => MonadError (Array Error) m
--   => MonadReader (Ctx m) m
--   => MonadState Env m
--   => MonadWriter (Array InterpretError) m
--   => Module
--   -> m Unit
-- interpretModule = todo ""
