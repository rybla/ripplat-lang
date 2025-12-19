module Ripplat.Thermodynamics where

import Prelude

import Control.Monad.State (StateT, evalStateT, gets)
import Control.Plus (empty)
import Data.Lens (view, (.=))
import Data.Lens.At (at)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Ripplat.Grammr (ColdConclusion, ColdId(..), ColdProp, ColdTm, ColdVar, HotConclusion, HotId(..), HotProp, HotRule, HotTm, HotVar, Prop(..), Rule, Rule'(..), Tm(..), Var(..), VarName)

--------------------------------------------------------------------------------

type HeatT = StateT (Map VarName (Map ColdId HotId))

runHeatT :: forall m a. Monad m => HeatT m a -> m a
runHeatT = (_ `evalStateT` empty)

heatRule :: forall m. Monad m => Rule -> HeatT m HotRule
heatRule (Rule r) = do
  hyps <- heatProp `traverse` r.hyps
  conc <- heatProp r.conc
  pure $ Rule { name: r.name, hyps, conc }

heatConclusion :: forall m. Monad m => ColdConclusion -> HeatT m HotConclusion
heatConclusion conc = do
  prop <- heatProp conc.prop
  pure { name: conc.name, prop }

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

coolRule :: forall m. Monad m => HotRule -> m Rule
coolRule (Rule r) = do
  hyps <- coolProp `traverse` r.hyps
  conc <- coolProp r.conc
  pure $ Rule { name: r.name, hyps, conc }

coolConclusion :: forall m. Monad m => HotConclusion -> m ColdConclusion
coolConclusion conc = do
  prop <- coolProp conc.prop
  pure { name: conc.name, prop }

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

