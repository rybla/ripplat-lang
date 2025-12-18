module Ripplat.Thermodynamics where

import Prelude

import Control.Monad.State (StateT, gets)
import Data.Lens (view, (.=))
import Data.Lens.At (at)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Ripplat.Grammr (ColdAxiom, ColdId(..), ColdLemma, ColdProp, ColdTm, ColdVar, HotId(..), HotLemma, HotProp, HotTm, HotVar, Prop(..), Tm(..), Var(..), VarName, HotAxiom)

--------------------------------------------------------------------------------

type HeatT = StateT (Map VarName (Map ColdId HotId))

heatLemma :: forall m. Monad m => ColdLemma -> HeatT m HotLemma
heatLemma lemma = do
  head <- heatProp lemma.head
  hyps <- heatProp `traverse` lemma.hyps
  conc <- heatProp lemma.conc
  pure { name: lemma.name, head, hyps, conc }

heatAxiom :: forall m. Monad m => ColdAxiom -> HeatT m HotAxiom
heatAxiom axiom = do
  conc <- heatProp axiom.conc
  pure { name: axiom.name, conc }

heatProp :: forall m. Monad m => ColdProp -> HeatT m HotProp
heatProp (Prop p) = do
  arg <- heatTm p.arg
  pure $ Prop { name: p.name, arg }

heatTm :: forall m. Monad m => ColdTm -> HeatT m HotTm
heatTm (VarTm v) = VarTm <$> heatVar v
heatTm UnitTm = pure $ UnitTm
heatTm (BoolTm b) = pure $ BoolTm b

heatVar :: forall m. Monad m => ColdVar -> HeatT m HotVar
heatVar (Var v) = at v.name # view # gets >>= case _ of
  -- we have NOT yet seen a variable with this name
  Nothing -> do
    let id = HotId 0
    at v.name .= Just (Map.singleton v.id id)
    pure $ Var { name: v.name, id }
  -- we have seen some variables with this name
  Just ids -> do
    case ids # Map.lookup v.id of
      -- we have NOT yet seen a variable with this name and id
      Nothing -> do
        let id = HotId $ Map.size ids
        pure $ Var { name: v.name, id }
      -- we have seen a variable with this name and id
      Just id -> pure $ Var { name: v.name, id }

--------------------------------------------------------------------------------

coolLemma :: forall m. Monad m => HotLemma -> m ColdLemma
coolLemma lemma = do
  head <- coolProp lemma.head
  hyps <- coolProp `traverse` lemma.hyps
  conc <- coolProp lemma.conc
  pure { name: lemma.name, head, hyps, conc }

coolAxiom :: forall m. Monad m => HotAxiom -> m ColdAxiom
coolAxiom axiom = do
  conc <- coolProp axiom.conc
  pure { name: axiom.name, conc }

coolProp :: forall m. Monad m => HotProp -> m ColdProp
coolProp (Prop p) = do
  arg <- coolTm p.arg
  pure $ Prop { name: p.name, arg }

coolTm :: forall m. Monad m => HotTm -> m ColdTm
coolTm (VarTm v) = VarTm <$> coolVar v
coolTm UnitTm = pure UnitTm
coolTm (BoolTm b) = pure $ BoolTm b

coolVar :: forall m. Monad m => HotVar -> m ColdVar
coolVar (Var v) = do
  id <- coolId v.id
  pure $ Var $ { name: v.name, id }

coolId :: forall m. Monad m => HotId -> m ColdId
coolId (HotId n) = pure $ ColdId (pure n)

