module Test.Examples.HouseOfSantaClaus (main, spec) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Data.Reflectable (reflectType)
import Data.Traversable (for_)
import Data.Variant (Variant)
import Effect (Effect)
import Effect.Aff (Aff)
import Test.Examples.Common (assertWalk, hasEulerTrail, (~>))
import Test.Spec (Spec)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Transit (type (:*), type (>|), Empty, match, mkUpdate, return)
import Transit.DSL (type (|<))
import Transit.Colors (themeHarmonyDark, themeHarmonyLight)
import Transit.Generators.Graphviz as TransitGraphviz
import Transit.Generators.TransitionTable as TransitTable
import Transit.StateGraph (mkStateGraph)
import Transit.VariantUtils (v)
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
  Empty
    :* ("N_1" |< "E_a" >| "N_2")
    :* ("N_2" |< "E_b" >| "N_3")
    :* ("N_3" |< "E_c" >| "N_5")
    :* ("N_5" |< "E_d" >| "N_4")
    :* ("N_4" |< "E_e" >| "N_1")
    :* ("N_1" |< "E_f" >| "N_3")
    :* ("N_2" |< "E_g" >| "N_4")
    :* ("N_3" |< "E_h" >| "N_4")

update :: State -> Msg -> State
update =
  mkUpdate @HouseOfSantaClausTransit
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

assert1 :: Aff Unit
assert1 =
  assertWalk update
    (v @"N_1")
    [ v @"E_f" ~> v @"N_3"
    , v @"E_h" ~> v @"N_4"
    , v @"E_g" ~> v @"N_2"
    , v @"E_a" ~> v @"N_1"
    , v @"E_e" ~> v @"N_4"
    , v @"E_d" ~> v @"N_5"
    , v @"E_c" ~> v @"N_3"
    , v @"E_b" ~> v @"N_2"
    ]

assert2 :: Aff Unit
assert2 =
  let
    graph = mkStateGraph (reflectType (Proxy @HouseOfSantaClausTransit))
  in
    hasEulerTrail graph `shouldEqual` true

spec :: Spec Unit
spec = do
  describe "House of Santa Claus" do
    it "asserts" do
      assert1
      assert2

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

