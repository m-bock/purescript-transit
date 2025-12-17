module Transit.Generators.TransitionTable
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
import Transit.Core (Match(..), Return(..), TransitCore(..))
import Transit.Data.Html as Html

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

getHasGuards :: Array Match -> Boolean
getHasGuards matches = Array.any
  ( \(Match _ _ returns) -> Array.any
      ( case _ of
          ReturnVia _ _ -> true
          Return _ -> false
      )
      returns
  )
  matches

mkMatch :: Options -> TransitCore -> Boolean -> Int -> Match -> Array Html.Node
mkMatch options transit hasGuards index (Match from msg returns) =
  case returns of
    [ Return to ] ->
      if options.useUndirectedEdges then
        case hasComplementaryEdge from to msg transit of
          Just i | i > index -> [ mkUndirectedRow options from msg to ]
          _ -> []
      else
        [ mkDirectedRow hasGuards options from msg (Return to) ]
    manyReturns ->
      map (mkDirectedRow hasGuards options from msg) manyReturns

mkUndirectedRow :: Options -> String -> String -> String -> Html.Node
mkUndirectedRow options fromState msg toState =
  Html.tbody []
    [ Html.tr []
        [ Html.td [] [ Html.text fromState ]
        , Html.td [] [ Html.text "⟵" ]
        , Html.td [] [ Html.text msg ]
        , Html.td [] [ Html.text "⟶" ]
        , Html.td [] [ Html.text toState ]
        ]
    ]

mkDirectedRow :: Boolean -> Options -> String -> String -> Return -> Html.Node
mkDirectedRow hasGuards options fromState msg ret =
  Html.tbody []
    [ Html.tr [] $ join
        [ pure $ Html.td [] [ Html.text fromState ]
        , pure $ Html.td [] [ Html.text "⟶" ]
        , pure $ Html.td [] [ Html.text msg ]
        , if hasGuards then case guard of
            Just guard ->
              [ Html.td [] [ Html.text "?" ]
              , Html.td [] [ Html.text guard ]
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
    ReturnVia guard to -> { toState: to, guard: Just guard }

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

type Options =
  { title :: Maybe String
  , useUndirectedEdges :: Boolean
  }

defaultOptions :: Options
defaultOptions =
  { title: Nothing
  , useUndirectedEdges: false
  }

writeToFile :: FilePath -> TransitCore -> (Options -> Options) -> Effect Unit
writeToFile path sg mkOptions = do
  FS.writeTextFile UTF8 path
    (Html.nodeToHtml $ toHtml (mkOptions defaultOptions) sg)
  Console.log $ "Wrote transition table to " <> path

writeToFile_ :: FilePath -> TransitCore -> Effect Unit
writeToFile_ path sg = writeToFile path sg identity

hasComplementaryEdge :: String -> String -> String -> TransitCore -> Maybe Int
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
