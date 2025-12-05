module Transit.Gen.TransitionTable where

import Prelude

import Data.Array (concatMap)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Reflectable (class Reflectable, reflectType)
import Effect (Effect)
import Effect.Class.Console as Console
import Node.Encoding (Encoding(..))
import Node.FS.Sync as FS
import Node.Path (FilePath)
import Transit.Html as Html
import Transit.Reflection as R
import Type.Proxy (Proxy(..))

data TransitionTable = TransitionTable (Array TableRow)
type TableRow = { fromState :: String, msg :: String, guard :: Maybe String, toState :: String }

mkTable :: Options -> R.StateGraph_ -> TransitionTable
mkTable _ (R.StateGraph transitions) = TransitionTable $ concatMap mkTableRow transitions

mkTableRow :: R.Transition_ -> Array TableRow
mkTableRow (R.Transition from msg returns) =
  map
    ( case _ of
        R.Return to -> { fromState: from, msg: msg, guard: Nothing, toState: to }
        R.ReturnVia guard to -> { fromState: from, msg: msg, guard: Just guard, toState: to }
    )
    returns

tableToHtml :: TransitionTable -> Html.Node
tableToHtml (TransitionTable rows) =
  Html.table []

    [ Html.caption [ Html.attrStyle "text-align: left; font-weight: bold;" ] [ Html.text "Transition Table" ]
    , Html.thead []
        [ Html.tr []
            [ Html.th [] [ Html.text "From State" ]
            , Html.th [] []
            , Html.th [] [ Html.text "Message" ]
            , Html.th [] []
            , Html.th [] [ Html.text "To State" ]
            ]
        ]
    , Html.tbody [] $ map mkRow rows
    ]

mkRow :: TableRow -> Html.Node
mkRow row =
  Html.tr []
    [ Html.td [] [ Html.text row.fromState ]
    , Html.td [] [ Html.text "⟶" ]
    , Html.td []
        [ Html.text case row.guard of
            Nothing -> row.msg
            Just guard -> row.msg <> "?" <> guard
        ]
    , Html.td [] [ Html.text "⟶" ]
    , Html.td [] [ Html.text row.toState ]
    ]

type Options =
  { title :: String
  }

defaultOptions :: Options
defaultOptions =
  { title: "Untitled"
  }

writeToFile :: R.StateGraph_ -> (Options -> Options) -> FilePath -> Effect Unit
writeToFile sg mkOptions path = do
  FS.writeTextFile UTF8 path
    (Html.nodeToHtml $ tableToHtml (mkTable (mkOptions defaultOptions) sg))
  Console.log $ "Wrote graphviz graph to " <> path

writeToFile_ :: R.StateGraph_ -> FilePath -> Effect Unit
writeToFile_ sg path = writeToFile sg identity path

