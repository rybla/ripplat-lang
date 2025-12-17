module Ripplat.Interpretation where

import Prelude

import Ripplat.Common (class ToError, Error, Log)
import Ripplat.Grammr (ColdProp, Module(..), Prop(..), PropName, Rule(..), RuleDef(..), RuleName)
import Ripplat.Platform (Platform)
import Utility (partitionEither, prop')
import Control.Monad.Error.Class (class MonadError)
import Control.Monad.Logger (class MonadLogger)
import Control.Monad.RWS (RWST)
import Control.Monad.State (gets)
import Control.Plus (empty)
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Lens (view, (.=))
import Data.List (List(..))
import Data.List.Lazy as LazyList
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (fromMaybe)
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

-- | Interpretation environment
-- | - lemmas are grouped by their first hypothesis's proposition name
-- | - axioms are grouped by their proposition name
type Env =
  { lemmaGroups :: Map PropName (Array Lemma)
  , axiomGroups :: Map PropName (Array Axiom)
  }

-- | A lemma is a rule that has at least one hypothesis, called the head hypothesis.
type Lemma =
  { name :: RuleName
  , head :: ColdProp
  , hyps :: List ColdProp
  , conc :: ColdProp
  }

-- | An axiom is a rule that has no hypotheses.
type Axiom =
  { name :: RuleName
  , conc :: ColdProp
  }

newEnv :: {} -> Env
newEnv _args =
  { lemmaGroups: empty
  , axiomGroups: empty
  }

newtype InterpretError = InterpretError
  { label :: String
  , source :: String
  , msg :: String
  }

newInterpretError :: String -> String -> String -> InterpretError
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
    lemmaGroups /\ axiomGroups = partitionEither
      ( \(RuleDef rd) ->
          let
            Rule r = rd.rule
          in
            case r.hyps of
              Cons head@(Prop p) hyps ->
                Left $ p.name /\ [ { name: r.name, head, hyps, conc: r.conc } :: Lemma ]
              Nil ->
                let
                  Prop p = r.conc
                in
                  Right $ p.name /\ [ { name: r.name, conc: r.conc } :: Axiom ]
      )
      md.ruleDefs
  prop' @"lemmaGroups" .= (lemmaGroups # Map.fromFoldableWith append)
  prop' @"axiomGroups" .= (axiomGroups # Map.fromFoldableWith append)
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
learn = do
  lemmaGroups <- gets $ view $ prop' @"lemmaGroups"
  axiomGroups <- gets $ view $ prop' @"axiomGroups"
  lemmaGroups # (Map.toUnfoldable :: _ -> LazyList.List _) # traverse_ \(propName /\ lemmas) -> do
    lemmas # traverse_ \lemma -> do
      axiomGroups # Map.lookup propName # fromMaybe mempty # traverse_ \axiom -> do
        applyLemmaToAxiom lemma axiom

  pure false

applyLemmaToAxiom
  :: forall m
   . MonadLogger Log m
  => MonadError (Array Error) m
  => Lemma
  -> Axiom
  -> T m Boolean
applyLemmaToAxiom lemma axiom = do
  if (lemma.head # unwrap).name == (axiom.conc # unwrap).name then
    pure false
  else
    pure false

