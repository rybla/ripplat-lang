module Ripplat.Interpretation where

import Prelude

import Control.Applicative (pure)
import Control.Monad.Except (class MonadError, runExceptT, throwError)
import Control.Monad.RWS (RWSResult(..), RWST, modify_)
import Control.Monad.Reader (asks)
import Control.Monad.State (execStateT, gets)
import Control.Monad.Trans.Class (lift)
import Control.Plus (empty)
import Data.Bifunctor (bimap)
import Data.Either (Either(..), either, isRight)
import Data.Either.Nested (type (\/))
import Data.Foldable (and, or, traverse_)
import Data.Lens (view, (.=))
import Data.Lens.At (at)
import Data.List (List(..))
import Data.List.Lazy as LazyList
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (fromMaybe, maybe, maybe')
import Data.Newtype (class Newtype, unwrap)
import Data.Tuple.Nested ((/\))
import Data.Unfoldable (none)
import Options.Applicative.Internal.Utils (unLines)
import Record as Record
import Ripplat.Checking as Checking
import Ripplat.Common (class ToError, Error(..), newError)
import Ripplat.Grammr (ColdId, ColdProp, ColdTm, HotId, Module(..), NormLat, Prop(..), PropDef(..), PropName, Rule(..), RuleDef(..), RuleName, Substitution)
import Ripplat.Platform (Platform)
import Ripplat.Unification (unify)
import Ripplat.Unification as Unification
import Text.Pretty (indent, quoteCode)
import Utility (partitionEither, prop', runRWST', todo, todoK)

--------------------------------------------------------------------------------

type T m = RWST (Ctx m) (Array InterpretError) Env m

type Ctx m =
  Checking.CtxK
    ( platform :: Platform m
    )

newCtx
  :: forall m r
   . Monad m
  => { platform :: Platform m
     , module_ :: Module
     | r
     }
  -> Ctx m
newCtx args =
  { platform: args.platform
  }
    # Record.union (Checking.newCtx args)

-- | Interpretation environment
-- | - lemmas are grouped by their first hypothesis's proposition name
-- | - axioms are grouped by their proposition name
type Env =
  { lemmaGroups :: Map PropName (Array ColdLemma)
  , axiomGroups :: Map PropName (Array ColdAxiom)
  }

-- | A lemma is a rule that has at least one hypothesis, called the head hypothesis.
type Lemma id =
  { name :: RuleName
  , head :: Prop id
  , hyps :: List (Prop id)
  , conc :: Prop id
  }

type ColdLemma = Lemma ColdId
type HotLemma = Lemma HotId

-- | An axiom is a rule that has no hypotheses.
type Axiom id =
  { name :: RuleName
  , conc :: Prop id
  }

type ColdAxiom = Axiom ColdId
type HotAxiom = Axiom HotId

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

-- | Interpret a module by learning every consequence of its rules.
interpretModule :: forall m. MonadError Error m => Module -> T m Unit
interpretModule (Module md) = do
  let
    lemmaGroups /\ axiomGroups = partitionEither
      ( \(RuleDef rd) ->
          let
            Rule r = rd.rule
          in
            case r.hyps of
              Cons head@(Prop p) hyps ->
                Left $ p.name /\ [ { name: r.name, head, hyps, conc: r.conc } :: ColdLemma ]
              Nil ->
                let
                  Prop p = r.conc
                in
                  Right $ p.name /\ [ { name: r.name, conc: r.conc } :: ColdAxiom ]
      )
      md.ruleDefs
  prop' @"lemmaGroups" .= (lemmaGroups # Map.fromFoldableWith append)
  prop' @"axiomGroups" .= (axiomGroups # Map.fromFoldableWith append)
  learnFixpoint

-- | Repeatedly learn new knowledge until no more progress can be made.
learnFixpoint :: forall m. MonadError Error m => T m Unit
learnFixpoint = do
  progress <- learn
  if progress then
    learnFixpoint
  else
    pure unit

-- | Learn the next generation of knowledge by attempting to apply each lemma to
-- | each axiom. Returns whether or not knowledge progress was made.
-- learn :: forall m. Monad m => StateT Boolean (T m) Unit
learn :: forall m. MonadError Error m => T m Boolean
learn = go `execStateT` false
  where
  go = do
    lemmaGroups <- lift $ gets $ view $ prop' @"lemmaGroups"
    axiomGroups <- lift $ gets $ view $ prop' @"axiomGroups"
    lemmaGroups # (Map.toUnfoldable :: _ -> LazyList.List _) # traverse_ \(propName /\ lemmas) -> do
      lemmas # traverse_ \lemma -> do
        axiomGroups # Map.lookup propName # fromMaybe mempty # traverse_ \axiom -> do
          progress <- lift $ applyLemmaToAxiom lemma axiom
          modify_ (progress || _)

applyLemmaToAxiom :: forall m. MonadError Error m => ColdLemma -> ColdAxiom -> T m Boolean
applyLemmaToAxiom lemma0 axiom0 = go lemma0 axiom0 # runExceptT >>= or >>> pure
  where
  go lemma axiom = do
    unless ((lemma.head # unwrap).name == (axiom.conc # unwrap).name) $ throwError false
    lemma' <- lift $ heatLemma lemma
    axiom' <- lift $ heatAxiom axiom
    RWSResult uniEnv uniResult _uniAssignments <- unify ((lemma'.conc # unwrap).arg /\ (axiom'.conc # unwrap).arg)
      # runExceptT
      # (_ `runRWST'` (Unification.newCtx {} /\ Unification.newEnv {}))
    unless (isRight uniResult) $ throwError false
    -- apply substitution to rest of lemma
    let lemma'' = substituteLemma uniEnv.sigma lemma'
    let delemma = decapitateLemma lemma''
    let delemma' = delemma # bimap freezeLemma freezeAxiom
    lift $ delemma' # either learnLemma learnAxiom

--------------------------------------------------------------------------------

-- | Learn a lemma into the state. Returns whether or not this resulted in
-- | knowledge progress (i.e. the lemma was not subsumed by existing knowledge).
learnLemma :: forall m. MonadError Error m => ColdLemma -> T m Boolean
learnLemma lemma = go # runExceptT >>= either pure (const (pure true))
  where
  go = do
    -- check if the lemma's conclusion is subsumed by a known axiom
    axioms <- lift $ gets $ view $ prop' @"axiomGroups" <<< at (lemma.conc # unwrap).name
    axioms # fromMaybe none # traverse_ \axiom -> do
      whenM (lift $ subsumedBy lemma.conc axiom.conc) do
        throwError false

-- | Learn a axiom into the state. Returns whether or not this resulted in
-- | knowledge progress (i.e. the axiom was not subsumed by existing knowledge).
learnAxiom :: forall m. MonadError Error m => ColdAxiom -> T m Boolean
learnAxiom axiom = go # runExceptT >>= either pure (const (pure true))
  where
  go = do
    -- check if the axioms is subsumed by a known axiom
    axioms <- lift $ gets $ view $ prop' @"axiomGroups" <<< at (axiom.conc # unwrap).name
    axioms # fromMaybe none # traverse_ \axiom' -> do
      whenM (lift $ subsumedBy axiom.conc axiom'.conc) do
        throwError false

-- | Whether or not the first proposition is subsumed by the second proposition.
subsumedBy :: forall m. MonadError Error m => ColdProp -> ColdProp -> T m Boolean
subsumedBy (Prop p1) (Prop p2) = do
  PropDef pd <- prop' @"propDefs" <<< at p1.name # view # asks >>= maybe (throwError $ newError [ "interpretation", "subsumedBy" ] $ "Reference to unknown proposition of the name " <> quoteCode (unwrap p1.name)) pure
  -- l <- ?a pd.param
  -- pure $ and
  --   [ p1.name == p2.name
  --   , latLeq l p1.arg p2.arg
  --   ]
  todo ""

latLeq :: NormLat -> ColdTm -> ColdTm -> Boolean
latLeq = todoK "latLeq"

--------------------------------------------------------------------------------

-- | Removes the head hypothesis of a lemma, which results in a stronger lemma
-- | (if there are other hypotheses) or an axiom (if there were no other
-- | hypotheses).
decapitateLemma :: forall id. Lemma id -> Lemma id \/ Axiom id
decapitateLemma lemma = case lemma.hyps of
  Cons h hyps -> Left { name: lemma.name, head: h, hyps, conc: lemma.conc }
  Nil -> Right { name: lemma.name, conc: lemma.conc }

--------------------------------------------------------------------------------

heatLemma :: forall m. Monad m => ColdLemma -> T m HotLemma
heatLemma = todoK "heatLemma"

heatAxiom :: forall m. Monad m => ColdAxiom -> T m HotAxiom
heatAxiom = todoK "heatAxiom"

freezeLemma :: HotLemma -> ColdLemma
freezeLemma = todoK "freezeLemma"

freezeAxiom :: HotAxiom -> ColdAxiom
freezeAxiom = todoK "freezeAxiom"

--------------------------------------------------------------------------------

substituteLemma :: Substitution -> HotLemma -> HotLemma
substituteLemma = todoK "substituteLemma"

substituteAxiom :: Substitution -> HotAxiom -> HotAxiom
substituteAxiom = todoK "substituteAxiom"
