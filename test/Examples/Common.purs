module Examples.Common where

import Prelude

import Data.Array (fromFoldable, scanl)
import Data.Array as Array
import Data.Foldable (foldl)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..), fst, snd)
import Data.Tuple.Nested (type (/\))
import Effect (Effect)
import Effect.Aff (Aff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Transit.Data.Graph as Graph
import Transit.StateGraph (StateNode, StateGraph)

nodeDegree :: StateGraph -> StateNode -> Int
nodeDegree graph node = Set.size (Graph.getOutgoingEdges node graph)

hasEulerTrail :: StateGraph -> Boolean
hasEulerTrail graph =
  let
    nodes :: Array StateNode
    nodes = fromFoldable (Graph.getNodes graph)

    countEdgesByNode :: Array Int
    countEdgesByNode = map (nodeDegree graph) nodes

    sumOddEdges :: Int
    sumOddEdges = Array.length (Array.filter Int.odd countEdgesByNode)
  in
    sumOddEdges == 2 || sumOddEdges == 0

infixr 4 Tuple as ~>

assertWalk
  :: forall msg state
   . Eq state
  => Show state
  => (state -> msg -> state)
  -> state
  -> Array (msg /\ state)
  -> Aff Unit
assertWalk updateFn initState walk = do
  let
    msgs :: Array msg
    msgs = map fst walk

    expectedStates :: Array state
    expectedStates = map snd walk

    actualStates :: Array state
    actualStates = scanl updateFn initState msgs

  actualStates `shouldEqual` expectedStates

-- | Generates all permutations of an array (all elements must be included).
-- | For example: permutations [1, 2, 3] = [[1,2,3], [1,3,2], [2,1,3], [2,3,1], [3,1,2], [3,2,1]]
permutations :: forall a. Ord a => Array a -> Set (Array a)
permutations xs = case Array.uncons xs of
  Nothing -> Set.singleton []
  Just { head, tail } ->
    let
      perms = permutations tail

      insertAtAllPositions :: Array a -> Set (Array a)
      insertAtAllPositions perm = case Array.uncons perm of
        Nothing -> Set.singleton [ head ]
        Just { head: first, tail: rest } ->
          Set.map (Array.cons first) (insertAtAllPositions rest) `Set.union` Set.singleton ([ head ] <> perm)
    in
      foldl (\acc perm -> acc `Set.union` insertAtAllPositions perm) Set.empty perms

assert1 :: Aff Unit
assert1 =
  permutations [ 1, 2, 3 ]
    `shouldEqual` Set.fromFoldable
      [ [ 1, 2, 3 ]
      , [ 1, 3, 2 ]
      , [ 2, 1, 3 ]
      , [ 2, 3, 1 ]
      , [ 3, 1, 2 ]
      , [ 3, 2, 1 ]
      ]

spec :: Spec Unit
spec = do
  describe "Common" do
    it "permutations" do
      assert1

main :: Effect Unit
main = do
  pure unit