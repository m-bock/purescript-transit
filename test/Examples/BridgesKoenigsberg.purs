module Test.Examples.BridgesKoenigsberg (main) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Reflectable (reflectType)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Transit (type (:*), type (:@), type (>|), Empty, Transit, match, mkUpdateGeneric, return)
import Transit.Generators.Graphviz as TransitGraphviz
import Transit.Generators.TransitionTable as TransitTable
import Transit.StateGraph (mkStateGraph)
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

type BridgesTransitions =
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
update = mkUpdateGeneric @BridgesTransitions
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

-- spec :: Spec Unit
-- spec = do
--   describe "Dead ends" do
--     it "should be empty" do
--       let r = reflectType (Proxy @DoorDSL)
--       let states = R.getStates r
--       let deadEnds = Array.filter (\x -> R.getOutgoing x r == []) states
--       deadEnds `shouldEqual` []

--------------------------------------------------------------------------------
--- State diagram generation
--------------------------------------------------------------------------------

graphOptions :: TransitGraphviz.Options -> TransitGraphviz.Options
graphOptions opts = opts
  { useUndirectedEdges = true
  }

tableOptions :: TransitTable.Options -> TransitTable.Options
tableOptions opts = opts
  { useUndirectedEdges = true
  }

main :: Effect Unit
main = do
  let
    transit = reflectType (Proxy @BridgesTransitions)

  TransitGraphviz.writeToFile graphOptions transit "graphs/bridges-koenigsberg.dot"
  TransitTable.writeToFile tableOptions transit "graphs/bridges-koenigsberg.html"

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
