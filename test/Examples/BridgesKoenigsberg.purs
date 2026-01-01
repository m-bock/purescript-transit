module Examples.BridgesKoenigsberg (main, spec) where

import Prelude

import Data.Array (foldM)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Reflectable (reflectType)
import Data.Traversable (for_)
import Data.Variant (Variant)
import Effect (Effect)
import Effect.Aff (Aff)
import Examples.Common (assertWalk, hasEulerTrail, permutations, (~>))
import Node.Encoding (Encoding(..))
import Node.FS.Sync as FS
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Transit (type (:*), type (>|), type (|<), StateGraph, Transit, TransitCore, match, mkStateGraph, mkUpdate, return)
import Transit.Render.Graphviz as TransitGraphviz
import Transit.Render.Theme (themeHarmonyDark, themeHarmonyLight)
import Transit.Render.TransitionTable as TransitTable
import Transit.VariantUtils (v)
import Type.Prelude (Proxy(..))

data MsgD = Cross_a | Cross_b | Cross_c | Cross_d | Cross_e | Cross_f | Cross_g

derive instance Eq MsgD
derive instance Ord MsgD

data StateD = LandA | LandB | LandC | LandD

updateClassic :: StateD -> MsgD -> Maybe StateD
updateClassic state msg = case state, msg of
  LandA, Cross_a -> Just LandB
  LandB, Cross_a -> Just LandA
  LandA, Cross_b -> Just LandB
  LandB, Cross_b -> Just LandA
  LandA, Cross_c -> Just LandC
  LandC, Cross_c -> Just LandA
  LandA, Cross_d -> Just LandC
  LandC, Cross_d -> Just LandA
  LandA, Cross_e -> Just LandD
  LandD, Cross_e -> Just LandA
  LandB, Cross_f -> Just LandD
  LandD, Cross_f -> Just LandB
  LandC, Cross_g -> Just LandD
  LandD, Cross_g -> Just LandC
  _, _ -> Nothing

--------------------------------------------------------------------------------
--- Transit Approach
--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------
--- Tests
--------------------------------------------------------------------------------

assert1 :: Aff Unit
assert1 =
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

bridgesKoenigsbergTransit :: TransitCore
bridgesKoenigsbergTransit = reflectType (Proxy @BridgesKoenigsbergTransit)

graph :: StateGraph
graph = mkStateGraph bridgesKoenigsbergTransit

assert2 :: Aff Unit
assert2 = do
  hasEulerTrail graph `shouldEqual` false

x :: Array (Maybe StateD)
x = do
  init <- [ LandA, LandB, LandC, LandD ]
  walk <- Array.fromFoldable $ permutations [ Cross_a, Cross_b, Cross_c, Cross_d, Cross_e, Cross_f, Cross_g ]
  pure $ foldM updateClassic init walk

-- assert4 :: Aff Unit
-- assert4 = do
--   Array.length (Array.mapMaybe (updateClassic LandA) (Array.fromFoldable allWalks))
--     `shouldEqual` 0

--   Array.length (Array.mapMaybe (updateClassic LandB) (Array.fromFoldable allWalks))
--     `shouldEqual` 0

--   Array.length (Array.mapMaybe (updateClassic LandC) (Array.fromFoldable allWalks))
--     `shouldEqual` 0

--   Array.length (Array.mapMaybe (updateClassic LandD) (Array.fromFoldable allWalks))
--     `shouldEqual` 0

spec :: Spec Unit
spec = do
  describe ".." do
    it "should assert1" do
      assert1
      assert2

--------------------------------------------------------------------------------
--- State diagram generation
--------------------------------------------------------------------------------

main :: Effect Unit
main = do
  let
    transit = reflectType (Proxy @BridgesKoenigsbergTransit)

  for_
    [ { theme: themeHarmonyLight, file: "renders/bridges-koenigsberg-light.dot" }
    , { theme: themeHarmonyDark, file: "renders/bridges-koenigsberg-dark.dot" }
    ]
    \opts -> do
      FS.writeTextFile UTF8 opts.file
        ( TransitGraphviz.generate transit _
            { useUndirectedEdges = true
            , theme = opts.theme
            }
        )

  FS.writeTextFile UTF8 "renders/bridges-koenigsberg.md"
    ( TransitTable.generate transit _
        { useUndirectedEdges = true
        , outputFormat = TransitTable.Markdown
        }
    )

