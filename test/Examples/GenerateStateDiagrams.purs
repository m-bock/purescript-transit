module Test.Examples.GenerateStateDiagrams (main) where

import Prelude

import Data.Reflectable (reflectType)
import Data.Traversable (for_)
import Effect (Effect)
import Test.Examples.SimpleDoor (SimpleDoorTransit)
import Transit.Colors (themeHarmonyDark, themeHarmonyLight)
import Transit.Generators.Graphviz as TransitGraphviz
import Type.Prelude (Proxy(..))

main :: Effect Unit
main = do
  let
    transit = reflectType (Proxy @SimpleDoorTransit)

  for_
    [ { theme: themeHarmonyLight, file: "graphs/simple-door-light.dot" }
    , { theme: themeHarmonyDark, file: "graphs/simple-door-dark.dot" }
    ]
    \opts ->
      TransitGraphviz.writeToFile opts.file transit _
        { title = "Simple Door"
        , theme = opts.theme
        }

