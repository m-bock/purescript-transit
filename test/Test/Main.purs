module Test.Main where

import Prelude

import Effect (Effect)
import Patchdown as Patchdown
import Test.Example as Example
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)
import Test.Transit.Core as Test.Transit.Core

main :: Effect Unit
main = do
  runSpecAndExitProcess [ consoleReporter ] Test.Transit.Core.spec
  Example.main
  Patchdown.main

