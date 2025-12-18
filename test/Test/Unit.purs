module Test.Unit where

import Prelude

import Control.Monad.Except (runExceptT, throwError)
import Data.Either (either)
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Ripplat.Grammr (Lat(..), Module(..), Ty'(..), Ty''(..), newLatDef, newPropDef)
import Ripplat.Main.Simple as Main.Simple
import Ripplat.Platform (mockPlatform)
import Test.Common as Common
import Test.Spec (Spec, describe, it)

spec :: Spec Unit
spec = describe "Unit" do

  it "ex1" $ Common.newSuccessTest $ do
    mb_errs <-
      Main.Simple.main mockPlatform
        ( Module
            { name: wrap "ex1"
            , tyDefs: []
            , latDefs: [ newLatDef (wrap "Lat1") [] (Ty' CanonicalLat UnitTy) ]
            , propDefs: [ newPropDef (wrap "Prop1") (RefTy (wrap "Lat1") []) ]
            , ruleDefs: []
            }
        )
        # runExceptT
        # map (either pure mempty)
    case mb_errs of
      Nothing -> pure unit
      Just errs -> throwError errs
    pure unit

  pure unit
