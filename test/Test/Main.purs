module Test.Main where

import Prelude

import Effect (Effect)
import Patchdown as Patchdown
import Test.Examples.ColorRing as Test.Examples.ColorRing
import Test.Examples.Door as Test.Examples.Door
import Test.Examples.DoorWithLock as Test.Examples.DoorWithLock
import Test.Examples.DoorWithPin as Test.Examples.DoorWithPin
import Test.Examples.BridgesKoenigsberg as Test.Examples.BridgesKoenigsberg
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)
import Test.Transit.Core as Test.Transit.Core

main :: Effect Unit
main = do
  runSpecAndExitProcess [ consoleReporter ] Test.Transit.Core.spec
  --Test.Examples.ColorRing.main
  Test.Examples.Door.main
  --Test.Examples.DoorWithLock.main
  --Test.Examples.DoorWithPin.main
  Test.Examples.BridgesKoenigsberg.main
  Patchdown.main

