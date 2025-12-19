module Test.Checking where

import Prelude

import Control.Monad.Except (class MonadError, throwError)
import Control.Monad.RWS (RWSResult(..), runRWST)
import Data.Foldable (null)
import Data.Newtype (wrap)
import Data.Tuple (Tuple(..))
import Effect.Exception as Exception
import Ripplat.Checking (checkModule, newCtx, newEnv)
import Ripplat.Common (newError, toError)
import Ripplat.Grammr (Lat(..), Module(..), Tm(..), Ty'(..), Ty''(..), newLatDef, newProp, newPropDef, newRule, newRuleDef)
import Test.Common as Common
import Test.Spec (Spec, describe, it)
import Utility (runRWST')

spec :: Spec Unit
spec = describe "Checking" do
  newSuccessTest "empty" $
    Module { name: wrap "empty", tyDefs: [], latDefs: [], propDefs: [], ruleDefs: [] }

  newSuccessTest "ex1" $
    Module
      { name: wrap "ex1"
      , tyDefs: []
      , latDefs: [ newLatDef (wrap "Lat1") [] (Ty' CanonicalLat UnitTy) ]
      , propDefs: [ newPropDef (wrap "Prop1") (RefTy (wrap "Lat1") []) ]
      , ruleDefs: []
      }

  newFailureTest "ex2" $
    Module
      { name: wrap "ex2"
      , tyDefs: []
      , latDefs: [ newLatDef (wrap "Lat1") [] (Ty' CanonicalLat UnitTy) ]
      , propDefs: [ newPropDef (wrap "Prop1") (RefTy (wrap "Lat666") []) ]
      , ruleDefs: []
      }

  newSuccessTest "ex3"
    $ Module
        { name: wrap "ex3"
        , tyDefs: []
        , latDefs: [ newLatDef (wrap "Lat1") [] (Ty' CanonicalLat UnitTy) ]
        , propDefs: [ newPropDef (wrap "Prop1") (RefTy (wrap "Lat1") []) ]
        , ruleDefs:
            [ newRuleDef $ newRule
                (wrap "Rule1")
                mempty
                (newProp (wrap "Prop1") UnitTm)
            ]
        }

  newFailureTest "ex5" $
    Module
      { name: wrap "ex5"
      , tyDefs: []
      , latDefs: [ newLatDef (wrap "Lat1") [] (Ty' CanonicalLat UnitTy) ]
      , propDefs: [ newPropDef (wrap "Prop1") (RefTy (wrap "Lat1") []) ]
      , ruleDefs:
          [ newRuleDef $ newRule
              (wrap "Rule1")
              mempty
              (newProp (wrap "Prop1") (BoolTm true))
          ]
      }

newSuccessTest :: String -> Module -> Spec Unit
newSuccessTest name md = Common.newSuccessTest name do
  RWSResult _ _ errs <- checkModule md
    `runRWST'`
      Tuple
        (newCtx { module_: md })
        (newEnv {})

  unless (null errs) do
    throwError $ map (toError [ "check" ]) errs

  pure unit

newFailureTest :: String -> Module -> Spec Unit
newFailureTest name md = Common.newSuccessTest name do
  RWSResult _ _ errs <-
    checkModule md
      `runRWST'`
        Tuple
          (newCtx { module_: md })
          (newEnv {})

  when (null errs) do
    throwError $ [ newError [ "check" ] "Expected errors" ]

  pure unit
