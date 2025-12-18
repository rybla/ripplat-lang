module Test.Unit where

import Prelude

import Control.Monad.Except (runExceptT, throwError)
import Data.Either (either)
import Data.List as List
import Data.Newtype (wrap)
import Data.String (joinWith)
import Effect.Aff.Class (liftAff)
import Ripplat.Grammr (Lat(..), Module(..), Tm(..), Ty'(..), Ty''(..), newLatDef, newProp, newPropDef, newRule, newRuleDef)
import Ripplat.Interpretation as Interpretation
import Ripplat.Main.Simple as Main.Simple
import Ripplat.Platform (mockPlatform)
import Test.Common as Common
import Test.Goldenfile (shouldEqualFile)
import Test.Spec (Spec, describe, it)

spec :: Spec Unit
spec = describe "Unit" do

  do
    let name = "ex1"
    it name $ Common.newSuccessTest $ do
      res <-
        Main.Simple.main
          { platform: mockPlatform
          , module_: Module
              { name: wrap name
              , tyDefs: []
              , latDefs: [ newLatDef (wrap "Lat1") [] (Ty' CanonicalLat UnitTy) ]
              , propDefs: [ newPropDef (wrap "Prop1") (RefTy (wrap "Lat1") []) ]
              , ruleDefs:
                  [ newRuleDef $ newRule
                      (wrap "Rule1")
                      ([] # List.fromFoldable)
                      (newProp (wrap "Prop1") UnitTm)
                  , newRuleDef $ newRule
                      (wrap "Rule2")
                      ([] # List.fromFoldable)
                      (newProp (wrap "Prop1") UnitTm)
                  ]
              }
          }
          # runExceptT
          >>= either throwError pure

      let report = Interpretation.prettyEnv res.interpretationEnv

      liftAff $ shouldEqualFile report $ joinWith "/" [ "golden", name <> ".interpretationReport.txt" ]

  pure unit
