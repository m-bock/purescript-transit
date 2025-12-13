module Docs.Main where

import Prelude

import Effect (Effect)
import Patchdown as Patchdown
import Test.Examples.SimpleDoor as Test.Examples.SimpleDoor
import Test.Examples.BridgesKoenigsberg as Test.Examples.BridgesKoenigsberg
import Test.Examples.ColorRing as Test.Examples.ColorRing
import Test.Examples.DoorWithPin as Test.Examples.DoorWithPin
import Test.Examples.HouseOfSantaClaus as Test.Examples.HouseOfSantaClaus
import Test.Spec (Spec)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)
import Test.Examples.Variants as Test.Examples.Variants

spec :: Spec Unit
spec = do
  Test.Examples.SimpleDoor.spec
  Test.Examples.DoorWithPin.spec
  Test.Examples.BridgesKoenigsberg.spec
  Test.Examples.HouseOfSantaClaus.spec
  Test.Examples.ColorRing.spec
  Test.Examples.Variants.spec

main :: Effect Unit
main = do
  runSpecAndExitProcess [ consoleReporter ] spec

  Test.Examples.SimpleDoor.main
  Test.Examples.DoorWithPin.main
  Test.Examples.BridgesKoenigsberg.main
  Test.Examples.HouseOfSantaClaus.main
  Test.Examples.ColorRing.main

  Patchdown.main