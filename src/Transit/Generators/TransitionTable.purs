module Transit.Generators.TransitionTable
  ( toHtml
  , writeToFile
  , writeToFile_
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class.Console as Console
import Node.Encoding (Encoding(..))
import Node.FS.Sync as FS
import Node.Path (FilePath)
import Transit.Core (Match(..), Return(..), TransitCore(..))
import Transit.Data.Html as Html

toHtml :: Options -> TransitCore -> Html.Node
toHtml options (TransitCore matches) = Html.table []
  [ Html.caption [] [ Html.text options.title ]
  , Html.thead [] [ mkHeader ]
  , Html.tbody [] $ Array.concatMap mkMatch matches
  ]

mkMatch :: Match -> Array Html.Node
mkMatch (Match from msg returns) =
  map (mkReturn from msg) returns

mkReturn :: String -> String -> Return -> Html.Node
mkReturn fromState msg ret =
  Html.tr []
    [ Html.td [] [ Html.text fromState ]
    , Html.td [] [ Html.text "⟶" ]
    , Html.td []
        [ Html.text
            ( case guard of
                Just guard -> msg <> " ? " <> guard
                Nothing -> msg
            )
        ]
    , Html.td [] [ Html.text "⟶" ]
    , Html.td [] [ Html.text toState ]
    ]
  where
  { toState, guard } = case ret of
    Return to -> { toState: to, guard: Nothing }
    ReturnVia guard to -> { toState: to, guard: Just guard }

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

writeToFile :: (Options -> Options) -> TransitCore -> FilePath -> Effect Unit
writeToFile mkOptions sg path = do
  FS.writeTextFile UTF8 path
    (Html.nodeToHtml $ toHtml (mkOptions defaultOptions) sg)
  Console.log $ "Wrote graphviz graph to " <> path

writeToFile_ :: TransitCore -> FilePath -> Effect Unit
writeToFile_ sg path = writeToFile identity sg path