module Docs.MainExamples where

import Prelude

import Effect (Effect)
import Test.Examples.SimpleDoor as Test.Examples.SimpleDoor
import Test.Examples.BridgesKoenigsberg as Test.Examples.BridgesKoenigsberg
import Test.Examples.ColorRing as Test.Examples.ColorRing
import Test.Examples.DoorWithPin as Test.Examples.DoorWithPin
import Test.Examples.HouseOfSantaClaus as Test.Examples.HouseOfSantaClaus
import Test.Examples.ErrorHandling as Test.Examples.ErrorHandling
import Test.Examples.ErrorHandling2 as Test.Examples.ErrorHandling2
import Test.Examples.Monadic as Test.Examples.Monadic
import Test.Examples.Signatures as Test.Examples.Signatures
import Test.Spec (Spec)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

spec :: Spec Unit
spec = do
  Test.Examples.SimpleDoor.spec
  Test.Examples.DoorWithPin.spec
  Test.Examples.BridgesKoenigsberg.spec
  Test.Examples.HouseOfSantaClaus.spec
  Test.Examples.ColorRing.spec
  Test.Examples.ErrorHandling.spec
  Test.Examples.ErrorHandling2.spec
  Test.Examples.Monadic.spec
  Test.Examples.Signatures.spec

runExamples :: Effect Unit
runExamples = do
  Test.Examples.SimpleDoor.main
  Test.Examples.DoorWithPin.main
  Test.Examples.BridgesKoenigsberg.main
  Test.Examples.HouseOfSantaClaus.main
  Test.Examples.ColorRing.main
  Test.Examples.ErrorHandling.main
  Test.Examples.ErrorHandling2.main
  Test.Examples.Monadic.main
  Test.Examples.Signatures.main

main :: Effect Unit
main = do
  runSpecAndExitProcess [ consoleReporter ] spec

  runExamples

