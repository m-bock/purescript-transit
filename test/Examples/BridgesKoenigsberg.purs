module Test.Examples.BridgesKoenigsberg (main, spec) where

import Prelude

import Data.Array (fromFoldable)
import Data.Array as Array
import Data.Generic.Rep (class Generic)
import Data.Int as Int
import Data.Reflectable (reflectType)
import Data.Set as Set
import Data.Show.Generic (genericShow)
import Data.Traversable (for_)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff)
import Test.Examples.Common (assertWalk, hasEulerTrail)
import Test.Spec (Spec)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldNotEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)
import Transit (type (:*), type (:@), type (>|), Empty, Transit, match, mkUpdateGeneric, return)
import Transit.Colors (themeHarmonyDark, themeHarmonyLight)
import Transit.Core (TransitCore(..))
import Transit.Data.Graph as Graph
import Transit.Generators.Graphviz as TransitGraphviz
import Transit.Generators.TransitionTable as TransitTable
import Transit.StateGraph (StateGraph, StateNode, mkStateGraph)
import Type.Function (type ($))
import Type.Prelude (Proxy(..))

--------------------------------------------------------------------------------
--- Types
--------------------------------------------------------------------------------

data State = LandA | LandB | LandC | LandD

data Msg
  = Cross_a
  | Cross_b
  | Cross_c
  | Cross_d
  | Cross_e
  | Cross_f
  | Cross_g

-- --------------------------------------------------------------------------------
-- --- TraditionalUpdate
-- --------------------------------------------------------------------------------

updateClassic :: State -> Msg -> State
updateClassic state msg = case state, msg of
  LandA, Cross_a -> LandB
  LandA, Cross_b -> LandB
  LandA, Cross_c -> LandC
  LandA, Cross_d -> LandC
  LandA, Cross_e -> LandD

  LandB, Cross_a -> LandA
  LandB, Cross_b -> LandA
  LandB, Cross_f -> LandD

  LandC, Cross_c -> LandA
  LandC, Cross_d -> LandA
  LandC, Cross_g -> LandD

  LandD, Cross_e -> LandA
  LandD, Cross_f -> LandB
  LandD, Cross_g -> LandC

  _, _ -> state

--------------------------------------------------------------------------------
--- transit Approach
--------------------------------------------------------------------------------

type BridgesKoenigsbergTransit =
  Transit $ Empty
    :* ("LandA" :@ "Cross_a" >| "LandB")
    :* ("LandB" :@ "Cross_a" >| "LandA")

    :* ("LandA" :@ "Cross_b" >| "LandB")
    :* ("LandB" :@ "Cross_b" >| "LandA")

    :* ("LandA" :@ "Cross_c" >| "LandC")
    :* ("LandC" :@ "Cross_c" >| "LandA")

    :* ("LandA" :@ "Cross_d" >| "LandC")
    :* ("LandC" :@ "Cross_d" >| "LandA")

    :* ("LandA" :@ "Cross_e" >| "LandD")
    :* ("LandD" :@ "Cross_e" >| "LandA")

    :* ("LandB" :@ "Cross_f" >| "LandD")
    :* ("LandD" :@ "Cross_f" >| "LandB")

    :* ("LandC" :@ "Cross_g" >| "LandD")
    :* ("LandD" :@ "Cross_g" >| "LandC")

update :: State -> Msg -> State
update = mkUpdateGeneric @BridgesKoenigsbergTransit
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

-- --------------------------------------------------------------------------------
-- --- Tests
-- --------------------------------------------------------------------------------

assert1 :: Aff Unit
assert1 =
  for_ [ updateClassic, update ] \fn ->
    assertWalk fn
      LandA
      [ Cross_a /\ LandB
      , Cross_f /\ LandD
      , Cross_g /\ LandC
      , Cross_c /\ LandA
      , Cross_e /\ LandD
      , Cross_g /\ LandC
      , Cross_d /\ LandA
      , Cross_b /\ LandB
      ]

bridgesKoenigsbergTransit :: TransitCore
bridgesKoenigsbergTransit = reflectType (Proxy @BridgesKoenigsbergTransit)

graph :: StateGraph
graph = mkStateGraph bridgesKoenigsbergTransit

assert2 :: Aff Unit
assert2 = do
  hasEulerTrail graph `shouldEqual` false

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
    [ { theme: themeHarmonyLight, file: "graphs/bridges-koenigsberg-light.dot" }
    , { theme: themeHarmonyDark, file: "graphs/bridges-koenigsberg-dark.dot" }
    ]
    \opts ->
      TransitGraphviz.writeToFile opts.file transit _
        { useUndirectedEdges = true
        , theme = opts.theme
        }

  TransitTable.writeToFile "graphs/bridges-koenigsberg.html" transit _
    { useUndirectedEdges = true }

--------------------------------------------------------------------------------
--- Instances
--------------------------------------------------------------------------------

derive instance Eq State
derive instance Eq Msg

derive instance Generic State _
derive instance Generic Msg _

instance Show State where
  show = genericShow

instance Show Msg where
  show = genericShow
