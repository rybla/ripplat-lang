module Ripplat.Interpretation where

import Prelude
import Ripplat.Common
import Ripplat.Grammr
import Ripplat.Platform
import Utility

import Control.Monad.Error.Class (class MonadError)
import Control.Monad.Logger (class MonadLogger)
import Control.Monad.RWS (RWST)
import Control.Plus (empty)
import Data.Either (Either(..))
import Data.Foldable (null)
import Data.Lens ((.=))
import Data.List (List(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Newtype (class Newtype, unwrap)
import Data.Tuple.Nested ((/\))
import Options.Applicative.Internal.Utils (unLines)
import Text.Pretty (indent)

--------------------------------------------------------------------------------

type T m = RWST (Ctx m) (Array InterpretError) Env m

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

type Env =
  { lemmas :: Map PropName (Array Lemma)
  , axioms :: Map PropName (Array Axiom)
  }

type Lemma = { name :: RuleName, hyp :: ColdProp, hyps :: List ColdProp, conc :: ColdProp }

type Axiom = { name :: RuleName, prop :: ColdProp }

newEnv :: {} -> Env
newEnv _args =
  { lemmas: empty
  , axioms: empty
  }

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

interpretModule
  :: forall m
   . MonadLogger Log m
  => MonadError (Array Error) m
  => Module
  -> T m Unit
interpretModule (Module md) = do
  let
    lemmas /\ axioms = partitionEither
      ( \(RuleDef rd) ->
          let
            Rule r = rd.rule
          in
            case r.hyps of
              Cons hyp@(Prop p) hyps ->
                Left $ p.name /\ [ { name: r.name, hyp, hyps, conc: r.conc } ]
              Nil ->
                let
                  Prop p = r.conc
                in
                  Right $ p.name /\ [ { name: r.name, prop: r.conc } ]
      )
      md.ruleDefs
  prop' @"lemmas" .= (lemmas # Map.fromFoldableWith append)
  prop' @"axioms" .= (axioms # Map.fromFoldableWith append)
  learnFixpoint

learnFixpoint
  :: forall m
   . MonadLogger Log m
  => MonadError (Array Error) m
  => T m Unit
learnFixpoint = do
  progress <- learn
  if progress then
    learnFixpoint
  else
    pure unit

learn
  :: forall m
   . MonadLogger Log m
  => MonadError (Array Error) m
  => T m Boolean
learn = todo ""

