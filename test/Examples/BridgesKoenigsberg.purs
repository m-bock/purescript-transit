module Examples.BridgesKoenigsberg (main, spec) where

import Prelude

import Data.Reflectable (reflectType)
import Data.Traversable (for_)
import Data.Variant (Variant)
import Effect (Effect)
import Effect.Aff (Aff)
import Examples.Common (assertWalk, hasEulerTrail, (~>))
import Node.Encoding (Encoding(..))
import Node.FS.Sync as FS
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Transit (type (:*), type (>|), type (|<), StateGraph, Transit, TransitCore, match, mkStateGraph, mkUpdate, return)
import Transit.Data.Table (Table(..))
import Transit.Data.Table as Table
import Transit.Render.Graphviz as TransitGraphviz
import Transit.Render.Theme (themeHarmonyDark, themeHarmonyLight)
import Transit.Render.TransitionTable as TransitTable
import Transit.VariantUtils (v)
import Type.Prelude (Proxy(..))

type State = Variant
  ( "LandA" :: {}
  , "LandB" :: {}
  , "LandC" :: {}
  , "LandD" :: {}
  )

type Msg = Variant
  ( "Cross_a" :: {}
  , "Cross_b" :: {}
  , "Cross_c" :: {}
  , "Cross_d" :: {}
  , "Cross_e" :: {}
  , "Cross_f" :: {}
  , "Cross_g" :: {}
  )

type BridgesKoenigsbergTransit =
  Transit
    :* ("LandA" |< "Cross_a" >| "LandB")
    :* ("LandA" |< "Cross_b" >| "LandB")
    :* ("LandA" |< "Cross_c" >| "LandC")
    :* ("LandA" |< "Cross_d" >| "LandC")
    :* ("LandA" |< "Cross_e" >| "LandD")
    :* ("LandB" |< "Cross_f" >| "LandD")
    :* ("LandC" |< "Cross_g" >| "LandD")

update :: State -> Msg -> State
update = mkUpdate @BridgesKoenigsbergTransit
  (match @"LandA" @"Cross_a" \_ _ -> return @"LandB")
  (match @"LandB" @"Cross_a" \_ _ -> return @"LandA")

  (match @"LandA" @"Cross_b" \_ _ -> return @"LandB")
  (match @"LandB" @"Cross_b" \_ _ -> return @"LandA")

  (match @"LandA" @"Cross_c" \_ _ -> return @"LandC")
  (match @"LandC" @"Cross_c" \_ _ -> return @"LandA")

  (match @"LandA" @"Cross_d" \_ _ -> return @"LandC")
  (match @"LandC" @"Cross_d" \_ _ -> return @"LandA")

  (match @"LandA" @"Cross_e" \_ _ -> return @"LandD")
  (match @"LandD" @"Cross_e" \_ _ -> return @"LandA")

  (match @"LandB" @"Cross_f" \_ _ -> return @"LandD")
  (match @"LandD" @"Cross_f" \_ _ -> return @"LandB")

  (match @"LandC" @"Cross_g" \_ _ -> return @"LandD")
  (match @"LandD" @"Cross_g" \_ _ -> return @"LandC")

bridgesKoenigsbergTransit :: TransitCore
bridgesKoenigsbergTransit = reflectType (Proxy @BridgesKoenigsbergTransit)

--------------------------------------------------------------------------------
--- Tests
--------------------------------------------------------------------------------

assert1 :: Spec Unit
assert1 =
  it "should follow the walk and visit the expected intermediate states" do
    assertWalk update
      (v @"LandA")
      [ v @"Cross_a" ~> v @"LandB"
      , v @"Cross_f" ~> v @"LandD"
      , v @"Cross_g" ~> v @"LandC"
      , v @"Cross_c" ~> v @"LandA"
      , v @"Cross_e" ~> v @"LandD"
      , v @"Cross_g" ~> v @"LandC"
      , v @"Cross_d" ~> v @"LandA"
      , v @"Cross_b" ~> v @"LandB"
      ]

graph :: StateGraph
graph = mkStateGraph bridgesKoenigsbergTransit

assert2 :: Aff Unit
assert2 = do
  hasEulerTrail graph `shouldEqual` false

-- assert4 :: Aff Unit
-- assert4 = do
--   Array.length (Array.mapMaybe (Classic.update Classic.LandA) (Array.fromFoldable allWalks))
--     `shouldEqual` 0

--   Array.length (Array.mapMaybe (Classic.update Classic.LandB) (Array.fromFoldable allWalks))
--     `shouldEqual` 0

--   Array.length (Array.mapMaybe (Classic.update Classic.LandC) (Array.fromFoldable allWalks))
--     `shouldEqual` 0

--   Array.length (Array.mapMaybe (Classic.update Classic.LandD) (Array.fromFoldable allWalks))
--     `shouldEqual` 0

spec :: Spec Unit
spec = do
  describe "BridgesKoenigsberg" do
    assert1

-- it "should assert1" do
--   assert2

--------------------------------------------------------------------------------
--- State diagram generation
--------------------------------------------------------------------------------

main :: Effect Unit
main = do

  for_
    [ { theme: themeHarmonyLight, file: "renders/bridges-koenigsberg-light.dot" }
    , { theme: themeHarmonyDark, file: "renders/bridges-koenigsberg-dark.dot" }
    ]
    \opts -> do
      FS.writeTextFile UTF8 opts.file
        ( TransitGraphviz.generate bridgesKoenigsbergTransit _
            { useUndirectedEdges = true
            , theme = opts.theme
            }
        )

  let
    table :: Table
    table = TransitTable.generate bridgesKoenigsbergTransit _
      { useUndirectedEdges = true
      }

  FS.writeTextFile UTF8
    "renders/bridges-koenigsberg.md"
    (Table.toMarkdown table)

