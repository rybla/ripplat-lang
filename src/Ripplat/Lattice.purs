module Ripplat.Lattice where

import Prelude
import Ripplat.Grammr
import Utility

-- | Check if the first term is lower than the second term in the lattice.
latLeq :: NormLat -> ColdTm -> ColdTm -> Boolean
latLeq (RefTy x _) _ _ = absurd x
-- 
latLeq _ (VarTm _) _ = true
latLeq _ _ (VarTm _) = false
-- 
latLeq (Ty' CanonicalLat l') t1 t2 = true
-- -- 
-- latLeq (Ty' l UnitTy) UnitTm UnitTm = true
-- latLeq (Ty' l UnitTy) _ _ = false
-- -- 
-- latLeq (Ty' l BoolTy) t1 t2 = todo "latLeq"
-- latLeq (Ty' l BoolTy) _ _ = false

