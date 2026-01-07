-- | Generator for HTML transition tables from transit specifications.
-- |
-- | This module converts state machine specifications into HTML tables
-- | showing all state transitions, supporting undirected edges and guard conditions.
module Transit.Render.TransitionTable
  ( toHtml
  , toTable
  , generate
  , generate_
  , Options
  , defaultOptions
  ) where

import Prelude

import Data.Array (catMaybes)
import Data.Array as Array
import Data.FunctorWithIndex (mapWithIndex)
import Data.Maybe (Maybe(..))
import Transit.Core (Match(..), MsgName, Return(..), StateName, TransitCore(..))
import Transit.Data.Html as Html
import Transit.Data.Table (Table)
import Transit.Data.Table as Table

-- | Generates an HTML table from a transit specification.
toHtml :: Options -> TransitCore -> Html.Node
toHtml options transit =
  let
    table = toTable options transit
    htmlTable = Table.toHtml table
  in
    case options.title of
      Just title ->
        case htmlTable of
          Html.Node "table" attrs children ->
            Html.table attrs $ [ Html.caption [] [ Html.text title ] ] <> children
          _ -> htmlTable
      Nothing -> htmlTable

-- | Generates a Table from a transit specification.
toTable :: Options -> TransitCore -> Table.Table
toTable options transit@(TransitCore matches) =
  Table.Table
    { headers: mkHeaders hasGuards
    , rows: join $ mapWithIndex (mkMatchRows options transit hasGuards) matches
    }
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
mkMatchRows :: Options -> TransitCore -> Boolean -> Int -> Match -> Array Table.TableRow
mkMatchRows options transit hasGuards index (Match from msg returns) =
  case returns of
    [ Return to ] ->
      if options.undirectedEdges then
        case hasComplementaryEdge from to msg transit of
          Just i | i > index -> [ mkUndirectedRow from msg to ]
          _ -> []
      else
        [ mkDirectedRow hasGuards from msg (Return to) ]
    manyReturns ->
      map (mkDirectedRow hasGuards from msg) manyReturns

-- | Creates a table row for an undirected (bidirectional) edge.
mkUndirectedRow :: StateName -> MsgName -> StateName -> Table.TableRow
mkUndirectedRow fromState msg toState =
  Table.TableRow
    [ Table.TableCell { bold: false } fromState
    , Table.TableCell { bold: true } "⟵"
    , Table.TableCell { bold: false } msg
    , Table.TableCell { bold: true } "⟶"
    , Table.TableCell { bold: false } toState
    ]

-- | Creates a table row for a directed edge, optionally including guard columns.
mkDirectedRow :: Boolean -> StateName -> MsgName -> Return -> Table.TableRow
mkDirectedRow hasGuards fromState msg ret =
  Table.TableRow $ join
    [ pure $ Table.TableCell { bold: false } fromState
    , pure $ Table.TableCell { bold: true } "⟶"
    , pure $ Table.TableCell { bold: false } msg
    , if hasGuards then
        case guard of
          Just guardValue ->
            [ Table.TableCell { bold: true } "?"
            , Table.TableCell { bold: false } guardValue
            ]
          Nothing ->
            [ Table.TableCell { bold: false } ""
            , Table.TableCell { bold: false } ""
            ]
      else
        []
    , pure $ Table.TableCell { bold: true } "⟶"
    , pure $ Table.TableCell { bold: false } toState
    ]
  where
  { toState, guard } = case ret of
    Return to -> { toState: to, guard: Nothing }
    ReturnVia guardValue to -> { toState: to, guard: Just guardValue }

-- | Creates the table headers, optionally including guard columns.
mkHeaders :: Boolean -> Array Table.TableHeader
mkHeaders hasGuards = join
  [ pure $ Table.TableHeader "State"
  , pure $ Table.TableHeader ""
  , pure $ Table.TableHeader "Message"
  , if hasGuards then
      [ Table.TableHeader ""
      , Table.TableHeader "Guard"
      ]
    else
      []
  , pure $ Table.TableHeader ""
  , pure $ Table.TableHeader "State"
  ]

-- | Configuration options for table generation.
type Options =
  { title :: Maybe String
  , undirectedEdges :: Boolean
  }

-- | Default options for table generation.
defaultOptions :: Options
defaultOptions =
  { title: Nothing
  , undirectedEdges: false
  }

-- | Generates a transition table with customizable options.
generate :: TransitCore -> (Options -> Options) -> Table
generate transitCore mkOptions =
  let
    options = mkOptions defaultOptions
  in
    toTable options transitCore

-- | Generates a transition table with default options.
generate_ :: TransitCore -> Table
generate_ transitCore = generate transitCore identity

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
