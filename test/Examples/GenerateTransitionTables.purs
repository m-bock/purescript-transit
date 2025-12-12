module Test.Examples.GenerateTransitionTables (main) where

import Prelude

import Data.Reflectable (reflectType)
import Effect (Effect)
import Test.Examples.SimpleDoor (SimpleDoorTransit)
import Transit.Generators.TransitionTable as TransitTable
import Type.Prelude (Proxy(..))

main :: Effect Unit
main = do
  let
    transit = reflectType (Proxy @SimpleDoorTransit)

  TransitTable.writeToFile "graphs/simple-door.html" transit _
    { title = "Simple Door" }

