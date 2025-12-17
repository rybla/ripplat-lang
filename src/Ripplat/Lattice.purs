module Ripplat.Lattice where

import Prelude

import Ripplat.Grammr (ColdTm, Lat(..), NormLat, Tm(..), Ty'(..), Ty''(..))

-- | Check if the first term is lower than the second term in the lattice.
latLeq :: NormLat -> ColdTm -> ColdTm -> Boolean
latLeq (RefTy x _) _ _ = absurd x
-- VarTm
latLeq _ (VarTm _) _ = true
latLeq _ _ (VarTm _) = false
-- CanonicalLat
latLeq (Ty' CanonicalLat UnitTy) UnitTm UnitTm = true
latLeq (Ty' CanonicalLat BoolTy) (BoolTm b1) (BoolTm b2) = b1 <= b2
latLeq (Ty' CanonicalLat _) _ _ = false

