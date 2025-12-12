module Test.Examples.GenerateStateDiagrams (main) where

import Prelude

import Data.Reflectable (reflectType)
import Effect (Effect)
import Test.Examples.Door (DoorTransit)
import Transit.Generators.Graphviz as TransitGraphviz
import Type.Prelude (Proxy(..))

main :: Effect Unit
main = do
  let
    transit = reflectType (Proxy @DoorTransit)

  TransitGraphviz.writeToFile "graphs/door.dot" transit _
    { title = "Door" }

