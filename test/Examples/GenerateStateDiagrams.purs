module Test.Examples.GenerateStateDiagrams (main) where

import Prelude

import Data.Reflectable (reflectType)
import Effect (Effect)
import Test.Examples.SimpleDoor (SimpleDoorTransit)
import Transit.Generators.Graphviz as TransitGraphviz
import Type.Prelude (Proxy(..))

main :: Effect Unit
main = do
  let
    transit = reflectType (Proxy @SimpleDoorTransit)

  TransitGraphviz.writeToFile "graphs/simple-door.dot" transit _
    { title = "Simple Door" }

