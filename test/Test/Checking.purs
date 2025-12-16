module Test.Checking where

import Prelude
import Ripplat.Checking
import Ripplat.Common
import Ripplat.Grammr
import Test.Spec

import Control.Monad.Except (class MonadError, throwError)
import Control.Monad.RWS (RWSResult(..), runRWST)
import Control.Monad.Reader (runReaderT)
import Control.Monad.State (evalStateT)
import Control.Monad.Writer (execWriterT)
import Data.Foldable (null)
import Data.Newtype (wrap)
import Effect.Exception as Exception
import Test.Common as Common
import Utility (todo)

spec :: Spec Unit
spec = describe "Checking" do
  it "empty" $ newSuccessTest (Module { name: wrap "empty", tyDefs: [], latDefs: [], propDefs: [], ruleDefs: [] })

  it "ex1" $ newSuccessTest
    ( Module
        { name: wrap "ex1"
        , tyDefs: []
        , latDefs: [ newLatDef (wrap "Lat1") [] (UnitTy CanonicalLat) ]
        , propDefs: [ newPropDef (wrap "Prop1") [ AppTy (wrap "Lat1") [] ] ]
        , ruleDefs: []
        }
    )

  it "ex2" $ newFailureTest
    ( Module
        { name: wrap "ex2"
        , tyDefs: []
        , latDefs: [ newLatDef (wrap "Lat1") [] (UnitTy CanonicalLat) ]
        , propDefs: [ newPropDef (wrap "Prop1") [ AppTy (wrap "Lat666") [] ] ]
        , ruleDefs: []
        }
    )

  it "ex3" $ newSuccessTest
    ( Module
        { name: wrap "ex3"
        , tyDefs: []
        , latDefs: [ newLatDef (wrap "Lat1") [] (UnitTy CanonicalLat) ]
        , propDefs: [ newPropDef (wrap "Prop1") [ AppTy (wrap "Lat1") [] ] ]
        , ruleDefs:
            [ newRuleDef $ newRule
                (wrap "Rule1")
                []
                (newProp (wrap "Prop1") [ UnitTm ])
            ]
        }
    )

  it "ex4" $ newFailureTest
    ( Module
        { name: wrap "ex4"
        , tyDefs: []
        , latDefs: [ newLatDef (wrap "Lat1") [] (UnitTy CanonicalLat) ]
        , propDefs: [ newPropDef (wrap "Prop1") [ AppTy (wrap "Lat1") [] ] ]
        , ruleDefs:
            [ newRuleDef $ newRule
                (wrap "Rule1")
                []
                (newProp (wrap "Prop1") [])
            ]
        }
    )

  it "ex5" $ newFailureTest
    ( Module
        { name: wrap "ex5"
        , tyDefs: []
        , latDefs: [ newLatDef (wrap "Lat1") [] (UnitTy CanonicalLat) ]
        , propDefs: [ newPropDef (wrap "Prop1") [ AppTy (wrap "Lat1") [] ] ]
        , ruleDefs:
            [ newRuleDef $ newRule
                (wrap "Rule1")
                []
                (newProp (wrap "Prop1") [ BoolTm true ])
            ]
        }
    )

newSuccessTest
  :: forall m
   . MonadError Exception.Error m
  => Module
  -> m Unit
newSuccessTest md = Common.newSuccessTest do
  RWSResult _ _ errs <- runRWST (checkModule md) (newCtx {}) (newEnv {})

  unless (null errs) do
    throwError $ map (toError [ "check" ]) errs

  pure unit

newFailureTest
  :: forall m
   . MonadError Exception.Error m
  => Module
  -> m Unit
newFailureTest md = Common.newSuccessTest do
  RWSResult _ _ errs <- runRWST (checkModule md) (newCtx {}) (newEnv {})

  when (null errs) do
    throwError $ [ newError [ "check" ] "Expected errors" ]

  pure unit
