module Test.Main where

import Prelude

import Effect (Effect)
import Test.Logger as Test.Logger
import Test.Spec.Reporter (specReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

main :: Effect Unit
main = runSpecAndExitProcess [ specReporter ] do
  Test.Logger.spec

