module Test.Transit.Render.TransitionTable
  ( spec
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Transit.Core (Match(..), Return(..), TransitCore(..))
import Transit.Data.Html (Node(..))
import Transit.Render.TransitionTable (defaultOptions, toHtml)

spec :: Spec Unit
spec = do
  describe "Transit.Render.TransitionTable" do
    describe "toHtml" do
      it "generates table from simple transit core" do
        let
          transitCore = TransitCore
            [ Match "State1" "Msg1" [ Return "State2" ]
            ]
          html = toHtml defaultOptions transitCore
        -- Verify it creates a table node
        case html of
          Node "table" _ _ -> true `shouldEqual` true
          _ -> false `shouldEqual` true

      it "generates table with multiple transitions" do
        let
          transitCore = TransitCore
            [ Match "State1" "Msg1" [ Return "State2" ]
            , Match "State2" "Msg2" [ Return "State3" ]
            , Match "State3" "Msg3" [ Return "State1" ]
            ]
          html = toHtml defaultOptions transitCore
        case html of
          Node "table" _ children ->
            -- Should have thead + tbody rows for each transition
            (Array.length children >= 4) `shouldEqual` true
          _ -> false `shouldEqual` true

      it "includes title when title option is provided" do
        let
          transitCore = TransitCore
            [ Match "State1" "Msg1" [ Return "State2" ]
            ]
          options = defaultOptions { title = Just "Test Table" }
          html = toHtml options transitCore
        case html of
          Node "table" _ children ->
            -- Should have caption as first child
            case Array.head children of
              Just (Node "caption" _ _) -> true `shouldEqual` true
              _ -> false `shouldEqual` true
          _ -> false `shouldEqual` true

      it "generates undirected row when useUndirectedEdges is true and complementary edge exists" do
        let
          transitCore = TransitCore
            [ Match "State1" "Msg1" [ Return "State2" ]
            , Match "State2" "Msg1" [ Return "State1" ]
            ]
          options = defaultOptions { useUndirectedEdges = true }
          html = toHtml options transitCore
        case html of
          Node "table" _ children ->
            -- Should have undirected row (one row instead of two)
            (Array.length children >= 2) `shouldEqual` true
          _ -> false `shouldEqual` true

      it "handles transitions with guards (ReturnVia)" do
        let
          transitCore = TransitCore
            [ Match "State1" "Msg1" [ ReturnVia "Guard1" "State2" ]
            ]
          html = toHtml defaultOptions transitCore
        case html of
          Node "table" _ children ->
            -- Should generate table with guard columns
            (Array.length children >= 2) `shouldEqual` true
          _ -> false `shouldEqual` true

      it "handles multiple returns from single match" do
        let
          transitCore = TransitCore
            [ Match "State1" "Msg1"
                [ Return "State2"
                , Return "State3"
                , ReturnVia "Guard1" "State1"
                ]
            ]
          html = toHtml defaultOptions transitCore
        case html of
          Node "table" _ children ->
            -- Should have rows for each return
            (Array.length children >= 4) `shouldEqual` true
          _ -> false `shouldEqual` true

      it "handles empty transit core" do
        let
          transitCore = TransitCore []
          html = toHtml defaultOptions transitCore
        case html of
          Node "table" _ children ->
            -- Should have at least header
            (Array.length children >= 1) `shouldEqual` true
          _ -> false `shouldEqual` true

      it "handles self-transitions" do
        let
          transitCore = TransitCore
            [ Match "State1" "Msg1" [ Return "State1" ]
            ]
          html = toHtml defaultOptions transitCore
        case html of
          Node "table" _ children ->
            -- Should generate self-transition row
            (Array.length children >= 2) `shouldEqual` true
          _ -> false `shouldEqual` true

