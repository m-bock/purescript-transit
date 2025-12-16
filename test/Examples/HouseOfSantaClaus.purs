module Test.Examples.HouseOfSantaClaus (main, spec) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Reflectable (reflectType)
import Data.Set as Set
import Data.Show.Generic (genericShow)
import Data.Traversable (for_, scanl)
import Data.Variant (Variant)
import Effect (Effect)
import Test.Examples.Common (assertWalk, hasEulerTrail, (~>))
import Test.Spec (Spec)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)
import Transit (type (:*), type (:@), type (>|), Empty, Transit, match, mkUpdate, mkUpdateGeneric, return)
import Transit.Colors (themeHarmonyDark, themeHarmonyLight)
import Transit.Data.Graph as Graph
import Transit.Generators.Graphviz as TransitGraphviz
import Transit.Generators.TransitionTable as TransitTable
import Transit.StateGraph (mkStateGraph)
import Transit.VariantUtils (inj)
import Type.Function (type ($))
import Type.Prelude (Proxy(..))

--------------------------------------------------------------------------------
--- transit Approach
--------------------------------------------------------------------------------

type State = Variant
  ( "N_1" :: {}
  , "N_2" :: {}
  , "N_3" :: {}
  , "N_4" :: {}
  , "N_5" :: {}
  )

type Msg = Variant
  ( "E_a" :: {}
  , "E_b" :: {}
  , "E_c" :: {}
  , "E_d" :: {}
  , "E_e" :: {}
  , "E_f" :: {}
  , "E_g" :: {}
  , "E_h" :: {}
  )

type HouseOfSantaClausTransit =
  Transit $ Empty
    :* ("N_1" :@ "E_a" >| "N_2")
    :* ("N_2" :@ "E_a" >| "N_1")

    :* ("N_2" :@ "E_b" >| "N_3")
    :* ("N_3" :@ "E_b" >| "N_2")

    :* ("N_3" :@ "E_c" >| "N_5")
    :* ("N_5" :@ "E_c" >| "N_3")

    :* ("N_5" :@ "E_d" >| "N_4")
    :* ("N_4" :@ "E_d" >| "N_5")

    :* ("N_4" :@ "E_e" >| "N_1")
    :* ("N_1" :@ "E_e" >| "N_4")

    :* ("N_1" :@ "E_f" >| "N_3")
    :* ("N_3" :@ "E_f" >| "N_1")

    :* ("N_2" :@ "E_g" >| "N_4")
    :* ("N_4" :@ "E_g" >| "N_2")

    :* ("N_3" :@ "E_h" >| "N_4")
    :* ("N_4" :@ "E_h" >| "N_3")

update :: State -> Msg -> State
update = mkUpdate @HouseOfSantaClausTransit
  (match @"N_1" @"E_a" \_ _ -> return @"N_2")
  (match @"N_2" @"E_a" \_ _ -> return @"N_1")

  (match @"N_2" @"E_b" \_ _ -> return @"N_3")
  (match @"N_3" @"E_b" \_ _ -> return @"N_2")

  (match @"N_3" @"E_c" \_ _ -> return @"N_5")
  (match @"N_5" @"E_c" \_ _ -> return @"N_3")

  (match @"N_5" @"E_d" \_ _ -> return @"N_4")
  (match @"N_4" @"E_d" \_ _ -> return @"N_5")

  (match @"N_4" @"E_e" \_ _ -> return @"N_1")
  (match @"N_1" @"E_e" \_ _ -> return @"N_4")

  (match @"N_1" @"E_f" \_ _ -> return @"N_3")
  (match @"N_3" @"E_f" \_ _ -> return @"N_1")

  (match @"N_2" @"E_g" \_ _ -> return @"N_4")
  (match @"N_4" @"E_g" \_ _ -> return @"N_2")

  (match @"N_3" @"E_h" \_ _ -> return @"N_4")
  (match @"N_4" @"E_h" \_ _ -> return @"N_3")

-- --------------------------------------------------------------------------------
-- --- Tests
-- --------------------------------------------------------------------------------

spec :: Spec Unit
spec = do
  describe "House of Santa Claus" do
    it "should have 8 states" do
      let transit = reflectType (Proxy @HouseOfSantaClausTransit)
      let graph = mkStateGraph transit

      let
        walk =
          [ inj @"E_f"
          , inj @"E_h"
          , inj @"E_g"
          , inj @"E_a"
          , inj @"E_e"
          , inj @"E_d"
          , inj @"E_c"
          , inj @"E_b"
          ]

      Array.length (Array.nub walk) `shouldEqual` 8

      foldl update (inj @"N_1") walk `shouldEqual` inj @"N_2"

      hasEulerTrail graph `shouldEqual` true
      pure unit

    describe "should follow the walk" do
      let
        initState = inj @"N_1"

        walk =
          [ inj @"E_f" ~> inj @"N_3"
          , inj @"E_h" ~> inj @"N_4"
          , inj @"E_g" ~> inj @"N_2"
          , inj @"E_a" ~> inj @"N_1"
          , inj @"E_e" ~> inj @"N_4"
          , inj @"E_d" ~> inj @"N_5"
          , inj @"E_c" ~> inj @"N_3"
          , inj @"E_b" ~> inj @"N_2"
          ]

      it "should follow the walk" do
        assertWalk update initState walk

--------------------------------------------------------------------------------
--- State diagram generation
--------------------------------------------------------------------------------

main :: Effect Unit
main = do
  let
    transit = reflectType (Proxy @HouseOfSantaClausTransit)
    nodeAttrs = Just \node -> case node of
      "N_1" -> "pos=\"0,0!\""
      "N_2" -> "pos=\"2,0!\""
      "N_3" -> "pos=\"2,2!\""
      "N_4" -> "pos=\"0,2!\""
      "N_5" -> "pos=\"1,3!\""
      _ -> ""
    globalAttrs = Just "layout=neato"

  for_
    [ { theme: themeHarmonyLight, file: "graphs/house-of-santa-claus-light.dot" }
    , { theme: themeHarmonyDark, file: "graphs/house-of-santa-claus-dark.dot" }
    ]
    \opts ->
      TransitGraphviz.writeToFile opts.file transit _
        { useUndirectedEdges = true
        , nodeAttrsRaw = nodeAttrs
        , globalAttrsRaw = globalAttrs
        , theme = opts.theme
        }

  TransitTable.writeToFile "graphs/house-of-santa-claus.html" transit _
    { useUndirectedEdges = true }

