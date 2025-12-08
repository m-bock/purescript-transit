module Transit.Generators.TransitionTable
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
import Transit.Core (TransitCore_)
import Transit.Data.Graph (Connection)
import Transit.Data.Graph as Graph
import Transit.Data.Html as Html
import Transit.StateGraph (Edge, Node, StateGraph(..))
import Unsafe.Coerce (unsafeCoerce)

toHtml :: Options -> TransitCore_ -> Html.Node
toHtml options _ = unsafeCoerce ""

-- Html.table []
--   [ Html.caption [] [ Html.text options.title ]
--   , Html.thead [] [ mkHeader ]
--   , Html.tbody [] $ map mkRow $ Set.toUnfoldable $ Graph.getConnections sg
--   ]

mkRow :: Connection Edge Node -> Html.Node
mkRow connection = Html.tr []
  [ Html.td [] [ Html.text connection.fromNode ]
  , Html.td [] [ Html.text "⟶" ]
  , Html.td []
      [ Html.text
          ( case connection.edge.guard of
              Just guard -> connection.edge.msg <> " ? " <> guard
              Nothing -> connection.edge.msg
          )
      ]
  , Html.td [] [ Html.text "⟶" ]
  , Html.td [] [ Html.text connection.toNode ]
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

writeToFile :: (Options -> Options) -> TransitCore_ -> FilePath -> Effect Unit
writeToFile mkOptions sg path = do
  FS.writeTextFile UTF8 path
    (Html.nodeToHtml $ toHtml (mkOptions defaultOptions) sg)
  Console.log $ "Wrote graphviz graph to " <> path

writeToFile_ :: TransitCore_ -> FilePath -> Effect Unit
writeToFile_ sg path = writeToFile identity sg path