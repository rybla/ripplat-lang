module Test.Unit where

import Prelude

import Control.Comonad (extract)
import Control.Monad.Except (ExceptT)
import Control.Monad.Writer (WriterT)
import Data.Foldable (foldl)
import Data.Lens ((<>~))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.List as List
import Data.Newtype (wrap)
import Data.String (joinWith)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Ripplat.Common (Log)
import Ripplat.Common as Ripplat.Common
import Ripplat.Grammr (Lat(..), Module(..), Tm(..), Ty'(..), Ty''(..), newLatDef, newProp, newPropDef, newRule, newRuleDef)
import Ripplat.Interpretation (Gas(..))
import Ripplat.Interpretation as Interpretation
import Ripplat.Main.Simple as Main.Simple
import Ripplat.Platform (Platform, mockPlatform)
import Test.Common as Common
import Test.Goldenfile (shouldEqualFile)
import Test.Spec (Spec, describe)
import Utility (prop')

spec :: Spec Unit
spec = describe "Unit" do

  newSuccessTest
    { name: "ex1"
    , platform: mockPlatform
    , gas: FiniteGas 20
    , module_: Module
        { name: wrap "Test"
        , tyDefs: []
        , latDefs: [ newLatDef (wrap "Lat1") [] (Ty' CanonicalLat UnitTy) ]
        , propDefs: [ newPropDef (wrap "Prop1") (RefTy (wrap "Lat1") []) ]
        , ruleDefs: []
        }
    }

  newSequentialSuccessTests
    { name: "ex2"
    , platform: mockPlatform
    , gas: FiniteGas 20
    , module_: Module
        { name: wrap "Test"
        , tyDefs: []
        , latDefs: []
        , propDefs: [ newPropDef (wrap "P") (Ty' CanonicalLat BoolTy) ]
        , ruleDefs: []
        }
    }
    [ _Newtype <<< prop' @"ruleDefs" <>~
        [ newRuleDef $ newRule
            (wrap "R1")
            ([ newProp (wrap "P") (BoolTm false) ] # List.fromFoldable)
            (newProp (wrap "P") (BoolTm true))
        ]
    ]

  pure unit

newSuccessTest
  :: forall r
   . { module_ :: Module
     , name :: String
     , platform :: Platform (ExceptT (Array Ripplat.Common.Error) (WriterT (Array Log) Aff))
     , gas :: Gas
     | r
     }
  -> Spec Unit
newSuccessTest args = do
  Common.newSuccessTest args.name do
    res <- Main.Simple.main args
    let report = Interpretation.prettyEnv res.interpretationEnv
    liftAff $ shouldEqualFile report $ joinWith "/" [ "golden", args.name <> ".interpretationReport.txt" ]

newSequentialSuccessTests
  :: forall r
   . { module_ :: Module
     , name :: String
     , platform :: Platform (ExceptT (Array Ripplat.Common.Error) (WriterT (Array Log) Aff))
     , gas :: Gas
     | r
     }
  -> Array (Module -> Module)
  -> Spec Unit
newSequentialSuccessTests args =
  let
    newName i = args.name <> "_step" <> show i
  in
    foldl
      ( \((i /\ md) /\ m) k ->
          let
            md' = k md
          in
            ((i + 1) /\ md') /\
              do
                m
                newSuccessTest
                  { name: newName i
                  , module_: md'
                  , platform: args.platform
                  , gas: args.gas
                  }

      )
      ( (1 /\ args.module_) /\ do
          newSuccessTest
            { name: newName 0
            , module_: args.module_
            , platform: args.platform
            , gas: args.gas
            }
      )
      >>> extract

