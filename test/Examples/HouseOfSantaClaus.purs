module Test.Examples.HouseOfSantaClaus (main) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Reflectable (reflectType)
import Data.Set as Set
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Test.Examples.Common (hasEulerCircle, hasEulerTrail)
import Test.Spec (Spec)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)
import Transit (type (:*), type (:@), type (>|), Empty, Transit, match, mkUpdateGeneric, return)
import Transit.Data.Graph as Graph
import Transit.Generators.Graphviz as TransitGraphviz
import Transit.Generators.TransitionTable as TransitTable
import Transit.StateGraph (mkStateGraph)
import Type.Function (type ($))
import Type.Prelude (Proxy(..))

--------------------------------------------------------------------------------
--- Types
--------------------------------------------------------------------------------

data State = N_1 | N_2 | N_3 | N_4 | N_5
data Msg
  = E_a
  | E_b
  | E_c
  | E_d
  | E_e
  | E_f
  | E_g
  | E_h

-- --------------------------------------------------------------------------------
-- --- TraditionalUpdate
-- --------------------------------------------------------------------------------

updateClassic :: State -> Msg -> State
updateClassic state msg = case state, msg of
  N_1, E_a -> N_2
  N_2, E_a -> N_1

  N_2, E_b -> N_3
  N_3, E_b -> N_2

  N_3, E_c -> N_5
  N_5, E_c -> N_3

  N_5, E_d -> N_4
  N_4, E_d -> N_5

  N_4, E_e -> N_1
  N_1, E_e -> N_4

  N_1, E_f -> N_3
  N_3, E_f -> N_1

  N_2, E_g -> N_4
  N_4, E_g -> N_2

  _, _ -> state

--------------------------------------------------------------------------------
--- transit Approach
--------------------------------------------------------------------------------

type TransitSantaClaus =
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
update = mkUpdateGeneric @TransitSantaClaus
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

walk :: Array Msg
walk = [ E_f, E_h, E_g, E_a, E_e, E_d, E_c, E_b ]

spec :: Spec Unit
spec = do
  describe ".." do
    it "..." do
      let transit = reflectType (Proxy @TransitSantaClaus)
      let graph = mkStateGraph transit

      let walk = [ E_f, E_h, E_g, E_a, E_e, E_d, E_c, E_b ]

      Array.length (Array.nub walk) `shouldEqual` 8

      foldl update N_1 walk `shouldEqual` N_2

      hasEulerCircle graph `shouldEqual` false
      hasEulerTrail graph `shouldEqual` true
      pure unit

--------------------------------------------------------------------------------
--- State diagram generation
--------------------------------------------------------------------------------

main :: Effect Unit
main = do
  let
    transit = reflectType (Proxy @TransitSantaClaus)

  TransitGraphviz.writeToFile "graphs/house-of-santa-claus.dot" transit _
    { useUndirectedEdges = true
    , nodeAttrsRaw = Just \node -> case node of
        "N_1" -> "pos=\"0,0!\""
        "N_2" -> "pos=\"2,0!\""
        "N_3" -> "pos=\"2,2!\""
        "N_4" -> "pos=\"0,2!\""
        "N_5" -> "pos=\"1,3!\""
        _ -> ""
    , globalAttrsRaw = Just "layout=neato"
    }

  TransitTable.writeToFile "graphs/house-of-santa-claus.html" transit _
    { useUndirectedEdges = true }

  runSpecAndExitProcess [ consoleReporter ] spec

--------------------------------------------------------------------------------
--- Instances
--------------------------------------------------------------------------------

derive instance Eq State
derive instance Eq Msg

derive instance Ord Msg

derive instance Generic State _
derive instance Generic Msg _

instance Show State where
  show = genericShow

instance Show Msg where
  show = genericShow
