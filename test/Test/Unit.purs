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
import Ripplat.Interpretation as Interpretation
import Ripplat.Main.Simple as Main.Simple
import Ripplat.Platform (Platform, mockPlatform)
import Test.Common as Common
import Test.Goldenfile (shouldEqualFile)
import Test.Spec (Spec, describe, it)
import Utility (prop')

spec :: Spec Unit
spec = describe "Unit" do

  newSuccessTest
    { name: "ex1"
    , platform: mockPlatform
    , module_: Module
        { name: wrap "Test"
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

  newSequentialSuccessTests
    { name: "ex2"
    , platform: mockPlatform
    , module_: Module
        { name: wrap "Test"
        , tyDefs: []
        , latDefs: []
        , propDefs: [ newPropDef (wrap "P") (Ty' CanonicalLat BoolTy) ]
        , ruleDefs:
            [ newRuleDef $ newRule
                (wrap "R1")
                ([] # List.fromFoldable)
                (newProp (wrap "P") (BoolTm false))
            ]
        }
    }
    [ _Newtype <<< prop' @"ruleDefs" <>~ []
    ]

  pure unit

newSuccessTest
  :: forall r
   . { module_ :: Module
     , name :: String
     , platform :: Platform (ExceptT (Array Ripplat.Common.Error) (WriterT (Array Log) Aff))
     | r
     }
  -> Spec Unit
newSuccessTest args = do
  it args.name $ Common.newSuccessTest $ do
    res <- Main.Simple.main args
    let report = Interpretation.prettyEnv res.interpretationEnv
    liftAff $ shouldEqualFile report $ joinWith "/" [ "golden", args.name <> ".interpretationReport.txt" ]

newSequentialSuccessTests
  :: forall r
   . { module_ :: Module
     , name :: String
     , platform :: Platform (ExceptT (Array Ripplat.Common.Error) (WriterT (Array Log) Aff))
     | r
     }
  -> Array (Module -> Module)
  -> Spec Unit
newSequentialSuccessTests args =
  foldl
    ( \((i /\ md) /\ m) k ->
        let
          md' = k md
          name = args.name <> "_step" <> show i
        in
          ((i + 1) /\ md') /\
            do
              m
              newSuccessTest
                { name
                , module_: md'
                , platform: args.platform
                }

    )
    ((0 /\ args.module_) /\ pure unit)
    >>> extract

