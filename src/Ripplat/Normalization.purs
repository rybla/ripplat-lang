module Ripplat.Normalization where

import Prelude

import Control.Monad.Except (class MonadError, throwError)
import Control.Monad.Reader (class MonadReader, asks)
import Data.Foldable (null)
import Data.Lens (view)
import Data.Lens.At (at)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (maybe)
import Data.Newtype (unwrap)
import Data.Tuple.Nested ((/\))
import Ripplat.Common (Error, newError)
import Ripplat.Grammr (LatDef(..), LatName, Module(..), NormLat, NormTy, PropDef, PropName, Ty'(..), Ty''(..), TyDef(..), TyName, WeirdLat, WeirdTy)
import Text.Pretty (quoteCode)
import Utility (prop', unimplemented)

--------------------------------------------------------------------------------

type Ctx r =
  { tyDefs :: Map TyName TyDef
  , latDefs :: Map LatName LatDef
  , propDefs :: Map PropName PropDef
  | r
  }

newCtx
  :: forall r
   . { module_ :: Module
     | r
     }
  -> Ctx ()
newCtx args =
  let
    Module md = args.module_
  in
    { tyDefs: md.tyDefs <#> (\it -> (unwrap it).name /\ it) # Map.fromFoldable
    , latDefs: md.latDefs <#> (\it -> (unwrap it).name /\ it) # Map.fromFoldable
    , propDefs: md.propDefs <#> (\it -> (unwrap it).name /\ it) # Map.fromFoldable
    }

--------------------------------------------------------------------------------

normalizeTy :: forall r m. MonadError (Array Error) m => MonadReader (Ctx r) m => WeirdTy -> m NormTy
normalizeTy (RefTy x ts) = do
  TyDef td <- prop' @"tyDefs" <<< at x # view # asks >>= maybe (throwError [ newError [ "check" ] $ "Reference to unknown type of the name " <> quoteCode (unwrap x) ]) pure
  if null ts then pure unit else unimplemented $ "actually need to do substituion of args for params here"
  normalizeTy td.ty
normalizeTy (Ty' _ UnitTy) = pure $ Ty' unit UnitTy
normalizeTy (Ty' _ BoolTy) = pure $ Ty' unit BoolTy

normalizeLatTy :: forall r m. MonadError (Array Error) m => MonadReader (Ctx r) m => WeirdLat -> m NormLat
normalizeLatTy (RefTy x ts) = do
  LatDef ld <- prop' @"latDefs" <<< at x # view # asks >>= maybe (throwError [ newError [ "check" ] $ "Reference to unknown type of the name " <> quoteCode (unwrap x) ]) pure
  if null ts then pure unit else unimplemented $ "actually need to do substituion of args for params here"
  normalizeLatTy ld.lat
normalizeLatTy (Ty' l UnitTy) = pure $ Ty' l UnitTy
normalizeLatTy (Ty' l BoolTy) = pure $ Ty' l BoolTy

