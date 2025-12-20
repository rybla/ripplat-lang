module Ripplat.Lattice where

import Prelude

import Ripplat.Grammr (ColdTm, Lat(..), NormLat, Tm(..), Ty'(..), Ty''(..))

-- | Check if the first term is lower than the second term in the lattice.
leqLat :: NormLat -> ColdTm -> ColdTm -> Boolean
leqLat (RefTy x _) _ _ = absurd x
-- VarTm
leqLat _ (VarTm _) _ = true
leqLat _ _ (VarTm _) = false
-- CanonicalLat
leqLat (Ty' CanonicalLat UnitTy) UnitTm UnitTm = true
leqLat (Ty' CanonicalLat BoolTy) (BoolTm b1) (BoolTm b2) = b1 <= b2
leqLat (Ty' CanonicalLat _) _ _ = false

