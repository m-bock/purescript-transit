module Test.Examples.GenerateStateDiagrams (main) where

import Prelude

import Data.Reflectable (reflectType)
import Effect (Effect)
import Test.Examples.Door (DoorDSL)
import Transit.Core (TransitCore_)
import Transit.Generators.Graphviz as TransitGraphviz
import Type.Prelude (Proxy(..))

main :: Effect Unit
main = do
  let
    transit :: TransitCore_
    transit = reflectType (Proxy @DoorDSL)

  TransitGraphviz.writeToFile (_ { title = "Door" }) transit "graphs/door.dot"

