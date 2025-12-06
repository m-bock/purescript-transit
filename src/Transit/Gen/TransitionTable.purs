module Transit.Gen.TransitionTable
  ( toHtml
  , writeToFile
  , writeToFile_
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Set as Set
import Effect (Effect)
import Effect.Class.Console as Console
import Node.Encoding (Encoding(..))
import Node.FS.Sync as FS
import Node.Path (FilePath)
import Transit.Graph as Graph
import Transit.Graph (Connection)
import Transit.Html as Html
import Transit.StateGraph (Edge, StateGraph, Node)

toHtml :: Options -> StateGraph -> Html.Node
toHtml _ sg = Html.table []
  [ Html.caption [] [ Html.text "Transition Table" ]
  , Html.thead [] [ mkHeader ]
  , Html.tbody [] $ map mkRow $ Set.toUnfoldable $ Graph.getEdges sg
  ]

mkRow :: Connection Edge Node -> Html.Node
mkRow connection = Html.tr []
  [ Html.td [] [ Html.text connection.fromNode.state ]
  , Html.td [] [ Html.text "⟶" ]
  , Html.td []
      [ Html.text
          ( connection.edge.msg <> case connection.edge.guard of
              Just guard -> "?" <> guard
              Nothing -> ""
          )
      ]
  , Html.td [] [ Html.text "⟶" ]
  , Html.td [] [ Html.text connection.toNode.state ]
  ]

mkHeader :: Html.Node
mkHeader = Html.tr []
  [ Html.th [] [ Html.text "From State" ]
  , Html.th [] []
  , Html.th [] [ Html.text "Message" ]
  , Html.th [] []
  , Html.th [] [ Html.text "To State" ]
  ]

type Options =
  { title :: String
  }

defaultOptions :: Options
defaultOptions =
  { title: "Untitled"
  }

writeToFile :: StateGraph -> (Options -> Options) -> FilePath -> Effect Unit
writeToFile sg mkOptions path = do
  FS.writeTextFile UTF8 path
    (Html.nodeToHtml $ toHtml (mkOptions defaultOptions) sg)
  Console.log $ "Wrote graphviz graph to " <> path

writeToFile_ :: StateGraph -> FilePath -> Effect Unit
writeToFile_ sg path = writeToFile sg identity path