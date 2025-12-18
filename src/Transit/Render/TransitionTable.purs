-- | Generator for HTML transition tables from transit specifications.
-- |
-- | This module converts state machine specifications into HTML tables
-- | showing all state transitions, supporting undirected edges and guard conditions.
module Transit.Render.TransitionTable
  ( toHtml
  , writeToFile
  , writeToFile_
  , Options
  , defaultOptions
  ) where

import Prelude

import Data.Array (catMaybes)
import Data.Array as Array
import Data.FunctorWithIndex (mapWithIndex)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class.Console as Console
import Node.Encoding (Encoding(..))
import Node.FS.Sync as FS
import Node.Path (FilePath)
import Transit.Core (Match(..), MsgName, Return(..), StateName, TransitCore(..))
import Transit.Data.Html as Html

-- | Generates an HTML table from a transit specification.
toHtml :: Options -> TransitCore -> Html.Node
toHtml options transit@(TransitCore matches) = Html.table []
  $ join
      [ case options.title of
          Just title -> [ Html.caption [] [ Html.text title ] ]
          Nothing -> []
      , pure $ Html.thead [] [ mkHeader hasGuards ]
      , join $ mapWithIndex (mkMatch options transit hasGuards) matches
      ]
  where
  hasGuards = getHasGuards matches

-- | Checks if any match in the array has guard conditions (ReturnVia).
getHasGuards :: Array Match -> Boolean
getHasGuards matches = Array.any
  ( \(Match _ _ returns) ->
      Array.any
        ( case _ of
            ReturnVia _ _ -> true
            Return _ -> false
        )
        returns
  )
  matches

-- | Creates table rows for a match, handling undirected edges and multiple returns.
mkMatch :: Options -> TransitCore -> Boolean -> Int -> Match -> Array Html.Node
mkMatch options transit hasGuards index (Match from msg returns) =
  case returns of
    [ Return to ] ->
      if options.useUndirectedEdges then
        case hasComplementaryEdge from to msg transit of
          Just i | i > index -> [ mkUndirectedRow from msg to ]
          _ -> []
      else
        [ mkDirectedRow hasGuards from msg (Return to) ]
    manyReturns ->
      map (mkDirectedRow hasGuards from msg) manyReturns

-- | Creates a table row for an undirected (bidirectional) edge.
mkUndirectedRow :: StateName -> MsgName -> StateName -> Html.Node
mkUndirectedRow fromState msg toState =
  Html.tbody []
    [ Html.tr []
        [ Html.td [] [ Html.text fromState ]
        , Html.td [] [ Html.text "⟵" ]
        , Html.td [] [ Html.text msg ]
        , Html.td [] [ Html.text "⟶" ]
        , Html.td [] [ Html.text toState ]
        ]
    ]

-- | Creates a table row for a directed edge, optionally including guard columns.
mkDirectedRow :: Boolean -> StateName -> MsgName -> Return -> Html.Node
mkDirectedRow hasGuards fromState msg ret =
  Html.tbody []
    [ Html.tr [] $ join
        [ pure $ Html.td [] [ Html.text fromState ]
        , pure $ Html.td [] [ Html.text "⟶" ]
        , pure $ Html.td [] [ Html.text msg ]
        , if hasGuards then
            case guard of
              Just guardValue ->
                [ Html.td [] [ Html.text "?" ]
                , Html.td [] [ Html.text guardValue ]
                ]
              Nothing ->
                [ Html.td [] []
                , Html.td [] []
                ]
          else
            []
        , pure $ Html.td [] [ Html.text "⟶" ]
        , pure $ Html.td [] [ Html.text toState ]
        ]
    ]
  where
  { toState, guard } = case ret of
    Return to -> { toState: to, guard: Nothing }
    ReturnVia guardValue to -> { toState: to, guard: Just guardValue }

-- | Creates the table header row, optionally including guard columns.
mkHeader :: Boolean -> Html.Node
mkHeader hasGuards = Html.tr [] $ join
  [ pure $ Html.th [] [ Html.text "State" ]
  , pure $ Html.th [] []
  , pure $ Html.th [] [ Html.text "Message" ]
  , if hasGuards then
      [ Html.th [] []
      , Html.th [] [ Html.text "Guard" ]
      ]
    else
      []
  , pure $ Html.th [] []
  , pure $ Html.th [] [ Html.text "State" ]
  ]

-- | Configuration options for table generation.
type Options =
  { title :: Maybe String
  , useUndirectedEdges :: Boolean
  }

-- | Default options for table generation.
defaultOptions :: Options
defaultOptions =
  { title: Nothing
  , useUndirectedEdges: false
  }

-- | Writes a transition table to a file with customizable options.
writeToFile :: FilePath -> TransitCore -> (Options -> Options) -> Effect Unit
writeToFile path transitCore mkOptions = do
  FS.writeTextFile UTF8 path
    (Html.nodeToHtml $ toHtml (mkOptions defaultOptions) transitCore)
  Console.log $ "Wrote transition table to " <> path

-- | Writes a transition table to a file with default options.
writeToFile_ :: FilePath -> TransitCore -> Effect Unit
writeToFile_ path transitCore = writeToFile path transitCore identity

-- | Checks if there exists a complementary edge (reverse direction with same message)
-- | and returns its index if found.
hasComplementaryEdge :: StateName -> StateName -> MsgName -> TransitCore -> Maybe Int
hasComplementaryEdge from to msg (TransitCore matches) =
  matches
    # mapWithIndex
        ( \i (Match from' msg' returns') ->
            if from' == to && msg' == msg && returns' == [ Return from ] then
              Just i
            else
              Nothing
        )
    # catMaybes
    # Array.head
