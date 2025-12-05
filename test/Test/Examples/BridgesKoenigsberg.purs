module Test.Examples.BridgesKoenigsberg where

import Prelude

import Data.Array as Array
import Data.Generic.Rep (class Generic)
import Data.Reflectable (reflectType)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Transit (type (:*), type (:@), type (>|), Empty, Wrap, match, mkUpdateGeneric, return')
import Transit.Gen.Graphviz as TransitGraphviz
import Transit.Gen.TransitionTable as TransitTable
import Transit.Reflection (addMeta)
import Transit.Reflection as R
import Type.Function (type ($))
import Type.Prelude (Proxy(..))

--------------------------------------------------------------------------------
--- Types
--------------------------------------------------------------------------------

data State = LandA | LandB | LandC | LandD

data Msg
  = CrossBridge_a
  | CrossBridge_b
  | CrossBridge_c
  | CrossBridge_d
  | CrossBridge_e
  | CrossBridge_f
  | CrossBridge_g

-- --------------------------------------------------------------------------------
-- --- TraditionalUpdate
-- --------------------------------------------------------------------------------

updateClassic :: State -> Msg -> State
updateClassic state msg = case state, msg of
  LandA, CrossBridge_a -> LandB
  LandA, CrossBridge_b -> LandB
  LandA, CrossBridge_c -> LandC
  LandA, CrossBridge_d -> LandC
  LandA, CrossBridge_e -> LandD

  LandB, CrossBridge_a -> LandA
  LandB, CrossBridge_b -> LandA
  LandB, CrossBridge_f -> LandD

  LandC, CrossBridge_c -> LandA
  LandC, CrossBridge_d -> LandA
  LandC, CrossBridge_g -> LandD

  LandD, CrossBridge_e -> LandA
  LandD, CrossBridge_f -> LandB
  LandD, CrossBridge_g -> LandC

  _, _ -> state

-- --------------------------------------------------------------------------------
-- --- transit Approach
-- --------------------------------------------------------------------------------

type BridgesTransitions =
  Wrap $ Empty
    :* ("LandA" :@ "CrossBridge_a" >| "LandB")
    :* ("LandA" :@ "CrossBridge_b" >| "LandB")
    :* ("LandA" :@ "CrossBridge_c" >| "LandC")
    :* ("LandA" :@ "CrossBridge_d" >| "LandC")
    :* ("LandA" :@ "CrossBridge_e" >| "LandD")

    :* ("LandB" :@ "CrossBridge_a" >| "LandA")
    :* ("LandB" :@ "CrossBridge_b" >| "LandA")
    :* ("LandB" :@ "CrossBridge_f" >| "LandD")

    :* ("LandC" :@ "CrossBridge_c" >| "LandA")
    :* ("LandC" :@ "CrossBridge_d" >| "LandA")
    :* ("LandC" :@ "CrossBridge_g" >| "LandD")

    :* ("LandD" :@ "CrossBridge_e" >| "LandA")
    :* ("LandD" :@ "CrossBridge_f" >| "LandB")
    :* ("LandD" :@ "CrossBridge_g" >| "LandC")

update :: State -> Msg -> State
update = mkUpdateGeneric @BridgesTransitions
  (match @"LandA" @"CrossBridge_a" \_ _ -> return' @"LandB")
  (match @"LandA" @"CrossBridge_b" \_ _ -> return' @"LandB")
  (match @"LandA" @"CrossBridge_c" \_ _ -> return' @"LandC")
  (match @"LandA" @"CrossBridge_d" \_ _ -> return' @"LandC")
  (match @"LandA" @"CrossBridge_e" \_ _ -> return' @"LandD")

  (match @"LandB" @"CrossBridge_a" \_ _ -> return' @"LandA")
  (match @"LandB" @"CrossBridge_b" \_ _ -> return' @"LandA")
  (match @"LandB" @"CrossBridge_f" \_ _ -> return' @"LandD")

  (match @"LandC" @"CrossBridge_c" \_ _ -> return' @"LandA")
  (match @"LandC" @"CrossBridge_d" \_ _ -> return' @"LandA")
  (match @"LandC" @"CrossBridge_g" \_ _ -> return' @"LandD")

  (match @"LandD" @"CrossBridge_e" \_ _ -> return' @"LandA")
  (match @"LandD" @"CrossBridge_f" \_ _ -> return' @"LandB")
  (match @"LandD" @"CrossBridge_g" \_ _ -> return' @"LandC")

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

-- --------------------------------------------------------------------------------
-- --- State diagram generation
-- --------------------------------------------------------------------------------

main :: Effect Unit
main = do
  let
    g = reflectType (Proxy @BridgesTransitions) # addMeta
      { name: "Bridges of Königsberg"
      , description: ""
      }
  TransitGraphviz.writeToFile_ g "graphs/bridges-koenigsberg.dot"
  TransitTable.writeToFile_ g "graphs/bridges-koenigsberg.html"

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
