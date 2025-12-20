module Ripplat.Interpretation where

import Prelude

import Control.Monad.Except (class MonadError, ExceptT, runExceptT, throwError)
import Control.Monad.Logger (class MonadLogger, log)
import Control.Monad.RWS (RWST, modify_)
import Control.Monad.Reader (ask, asks, runReaderT)
import Control.Monad.State (StateT, execStateT, gets)
import Control.Monad.Trans.Class (lift)
import Control.Plus (empty)
import Data.Bifunctor (rmap)
import Data.Either (Either(..), either)
import Data.Either.Nested (type (\/))
import Data.Foldable (and, fold, foldM, null, traverse_)
import Data.Lens (view, (.=))
import Data.Lens.At (at)
import Data.Lens.Index (ix)
import Data.List (List(..))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Traversable (traverse)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Options.Applicative.Internal.Utils (unLines)
import Record as Record
import Ripplat.Checking as Checking
import Ripplat.Common (class ToError, Error, Log, newError, newLog)
import Ripplat.Grammr (ColdConclusion, ColdProp, Module(..), Prop(..), PropDef(..), PropName, Rule, Rule'(..), RuleDef(..), WeirdLat, minProp, prettyConclusion, substituteRule)
import Ripplat.Lattice (leqLat)
import Ripplat.Normalization (normalizeLatTy)
import Ripplat.Platform (Platform)
import Ripplat.Thermodynamics (coolRule, heatConclusion, heatRule, runHeatT)
import Ripplat.Unification (unifyProps)
import Ripplat.Unification as Unification
import Text.Pretty (indent, indentBullet, pretty, quoteCode)
import Utility (partitionEither, prop')

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
-- | - concs are grouped by their proposition name
type Env =
  { gas :: Gas
  , rules :: Array Rule
  , concs :: Map PropName ColdConclusion
  }

newEnv
  :: forall r
   . { gas :: Gas
     | r
     }
  -> Env
newEnv args =
  { gas: args.gas
  , rules: empty
  , concs: empty
  }

prettyEnv :: Env -> String
prettyEnv env =
  unLines $
    [ indentBullet <<< unLines $
        [ "rules"
        , unLines $
            env.rules # map (indentBullet <<< pretty)

        ]
    , indentBullet <<< unLines $
        [ "concs"
        , unLines $
            env.concs
              # (Map.toUnfoldable :: _ -> Array _)
              # map \(_name /\ conc) -> indentBullet $ prettyConclusion $ conc

        ]
    ]

data Gas = FiniteGas Int | InfiniteGas

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
interpretModule :: forall m. MonadError (Array Error) m => MonadLogger Log m => Module -> T m Unit
interpretModule (Module md) = do
  concs0 <- md.propDefs
    # traverse
        ( \(PropDef pd) -> do
            conc <- minConclusion pd.name pd.param
            pure $ pd.name /\ [ conc ]
        )
    # map Map.fromFoldable
  let
    rules /\ concs =
      md.ruleDefs
        # partitionEither
            ( \(RuleDef rd) ->
                let
                  Rule r = rd.rule
                in
                  if null r.hyps then
                    let
                      Prop p = r.conc
                    in
                      Right $ p.name /\ [ { name: r.name, prop: r.conc } ]
                  else
                    Left [ rd.rule ]
            )
        # rmap (Map.fromFoldableWith append >>> Map.unionWith append concs0)
  prop' @"rules" .= fold rules
  (prop' @"concs" .= _) =<< initializeConcs concs
  learnFixpoint

initializeConcs
  :: forall m
   . MonadLogger Log m
  => MonadError (Array Error) m
  => Map PropName (Array ColdConclusion)
  -> T m (Map PropName ColdConclusion)
initializeConcs concGroups = do
  concGroups # traverseWithIndex \name concs -> do
    PropDef pd <- prop' @"propDefs" <<< at name # view # asks >>= maybe (throwError [ newError [ "interpretation", "initializeConcs" ] $ "Reference to unknown proposition " <> quoteCode (pretty name) ]) pure
    let go c1 c2 = c1.prop `subsumedBy` c2.prop >>= if _ then pure c1 else pure c2
    c <- minConclusion name pd.param
    concs # foldM go c

minConclusion
  :: forall m
   . MonadError (Array Error) m
  => MonadLogger Log m
  => PropName
  -> WeirdLat
  -> T m ColdConclusion
minConclusion name l = do
  l' <- (normalizeLatTy l # runReaderT) =<< ask
  pure $ { name: wrap $ "Bottom of " <> unwrap name, prop: minProp name l' }

-- | Repeatedly learn new knowledge until no more progress can be made.
learnFixpoint :: forall m. MonadError (Array Error) m => MonadLogger Log m => T m Unit
learnFixpoint = do
  progress <- learn
  if progress then
    learnFixpoint
  else
    pure unit

-- | Learn the next generation of knowledge by attempting to apply each lemma to
-- | each conc. Returns whether or not knowledge progress was made.
-- learn :: forall m. Monad m => StateT Boolean (T m) Unit
learn :: forall m. MonadError (Array Error) m => MonadLogger Log m => T m Boolean
learn = go `execStateT` false
  where
  go = do
    rules <- lift $ gets $ view $ prop' @"rules"
    rules # traverse_ applyRule

applyRule :: forall m. MonadError (Array Error) m => MonadLogger Log m => Rule -> StateT Boolean (T m) Unit
applyRule rule = do
  void $ lift (prop' @"gas" # view # gets) >>= case _ of
    InfiniteGas -> pure unit
    FiniteGas g | g <= 0 -> throwError [ newError [ "interpretation", "applyRule" ] $ unLines [ "Ran out of gas when applying rule:", indent (pretty rule) ] ]
    FiniteGas g -> lift $ prop' @"gas" .= FiniteGas (g - 1)
  log $ newLog [ "interpretation", "applyRule" ] $ pretty rule
  applyRule' rule # runExceptT # lift >>=
    traverse_ case _ of
      Left rule' -> applyRule rule'
      Right conc -> do
        progress <- lift $ learnConclusion conc
        modify_ (_ || progress)

applyRule' :: forall m. MonadError (Array Error) m => MonadLogger Log m => Rule -> ExceptT Unit (T m) (Rule \/ ColdConclusion)
applyRule' rule@(Rule r) = do
  case r.hyps of
    Nil -> do
      log $ newLog [ "interpretation", "applyRule'" ] $ "Satisfied all rule hypotheses of rule " <> quoteCode (pretty r.name) <> " to learn conclusion " <> quoteCode (pretty r.conc)
      pure $ Right { name: r.name, prop: r.conc }
    -- Cons (Prop p0) _ -> prop' @"concs" <<< ix p0.name # view # gets >>= \conc -> do
    --   -- OPTIMIZATION: check if at this point, the rule's conclusion is already subsumed by a known conclusion
    --   Rule r' /\ conc' <- runHeatT $ Tuple <$> heatRule rule <*> heatConclusion conc
    --   { head: hyp', tail: hyps' } <- r'.hyps # List.uncons # maybe (throwError [ newError [ "applyRule'" ] $ "impossible: r.hyps must be non-empty" ]) pure # lift
    --   sigma /\ res <- unifyProps hyp' conc'.prop # Unification.runT
    --   case res of
    --     Left err -> do
    --       log $ newLog [ "interpretation", "applyRule'" ] $ "Failed to unify rule's next hypothesis " <> quoteCode (pretty hyp') <> " with conclusion " <> quoteCode (prettyConclusion conc) <> " because " <> pretty err
    --       throwError unit
    --     Right _ -> do
    --       log $ newLog [ "interpretation", "applyRule'" ] $ "Succeeded to unify rule's next hypothesis " <> quoteCode (pretty hyp') <> " with conclusion " <> quoteCode (prettyConclusion conc)
    --       map Left $
    --         Rule r' { hyps = hyps' }
    --           # (substituteRule >>> flip runReaderT sigma)
    --           >>= coolRule
    Cons (Prop p0) _ -> do
      conc <- prop' @"concs" <<< at p0.name # view # gets >>= maybe (lift $ throwError [ newError [ "interpretation", "applyRule'" ] $ "No known conclusion for proposition " <> quoteCode (pretty p0.name) ]) pure
      -- OPTIMIZATION: check if at this point, the rule's conclusion is already subsumed by a known conclusion
      Rule r' /\ conc' <- runHeatT $ Tuple <$> heatRule rule <*> heatConclusion conc
      { head: hyp', tail: hyps' } <- r'.hyps # List.uncons # maybe (throwError [ newError [ "applyRule'" ] $ "impossible: r.hyps must be non-empty" ]) pure # lift
      sigma /\ res <- unifyProps hyp' conc'.prop # Unification.runT
      case res of
        Left err -> do
          log $ newLog [ "interpretation", "applyRule'" ] $ "Failed to unify rule's next hypothesis " <> quoteCode (pretty hyp') <> " with conclusion " <> quoteCode (prettyConclusion conc) <> " because " <> pretty err
          throwError unit
        Right _ -> do
          log $ newLog [ "interpretation", "applyRule'" ] $ "Succeeded to unify rule's next hypothesis " <> quoteCode (pretty hyp') <> " with conclusion " <> quoteCode (prettyConclusion conc)
          map Left $
            Rule r' { hyps = hyps' }
              # (substituteRule >>> flip runReaderT sigma)
              >>= coolRule

--------------------------------------------------------------------------------

-- | Learn a conclusion into the state. Returns whether or not this resulted in
-- | knowledge progress (i.e. the conclusion was not subsumed by existing
-- | knowledge).
learnConclusion :: forall m. MonadError (Array Error) m => MonadLogger Log m => ColdConclusion -> T m Boolean
learnConclusion conc = go # runExceptT >>= either (const (pure false)) (const (pure true))
  where
  go = do
    -- check if the concs is subsumed by a known conclusion
    prop' @"concs" <<< at (conc.prop # unwrap).name # view # gets # lift >>= case _ of
      Nothing -> pure unit
      Just conc' -> do
        whenM (lift $ subsumedBy conc.prop conc'.prop) do
          log $ newLog [ "interpretation", "learnConclusion" ] $ "Ignored candidate conclusion " <> quoteCode (prettyConclusion conc) <> " because it is already subsumed by known conclusion " <> quoteCode (prettyConclusion conc')
          throwError unit
    log $ newLog [ "interpretation", "learnConclusion" ] $ "Learned new conclusion " <> quoteCode (prettyConclusion conc)
    lift $ prop' @"concs" <<< ix (conc.prop # unwrap).name .= conc

-- | Check if the first proposition is subsumed by the second proposition.
subsumedBy :: forall m. MonadError (Array Error) m => MonadLogger Log m => ColdProp -> ColdProp -> T m Boolean
subsumedBy (Prop p1) (Prop p2) = do
  PropDef pd <- prop' @"propDefs" <<< at p1.name # view # asks >>= maybe (throwError [ newError [ "interpretation", "subsumedBy" ] $ "Reference to unknown proposition " <> quoteCode (unwrap p1.name) ]) pure
  l <- normalizeLatTy pd.param
  let
    result = and
      [ p1.name == p2.name
      , leqLat l p1.arg p2.arg
      ]
  log $ newLog [ "interpretation", "subsumedBy" ] $ pretty (Prop p1) <> " ?<= " <> pretty (Prop p2) <> " ==> " <> pretty result
  pure result

