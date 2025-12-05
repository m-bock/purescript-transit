module Transit.Gen.Graphviz where

import Prelude

import Color (Color)
import Color as Color
import Data.Array (catMaybes, concatMap, mapWithIndex, (!!))
import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Reflectable (class Reflectable, reflectType)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Class.Console as Console
import Node.Encoding (Encoding(..))
import Node.FS.Sync as FS
import Node.Path (FilePath)
import Transit.Colors (Colors, getColor)
import Transit.DotLang (Edge(..), GraphvizGraph(..), Node(..), Section(..), rankDirTD, toText)
import Transit.DotLang as D
import Transit.Reflection (Return_(..))
import Transit.Reflection as R
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

mkGraphvizGraph :: Options -> R.StateGraph_ -> GraphvizGraph
mkGraphvizGraph options sg@(R.StateGraph transitions) = GraphvizGraph $ join
  [ pure $ SecGlobal $
      D.GlobalAttrs
        [ D.rankDirTD
        , D.fontNameArial
        , D.labelHtmlBold options.title
        , D.labelLocT
        , D.fontSize 12
        ]
  , join $ mapWithIndex (mkNode sg) (getStates sg)
  -- , getEdges sg
  ]

mkNode :: R.StateGraph_ -> Int -> String -> Array Section
mkNode sg i stateName =
  join
    [ pure $ SecNode $ Node stateName
        [ D.shapeBox
        , D.labelHtmlBold stateName
        , D.fontSize 12
        , D.styleFilled
        , D.fillColor color.nodeBg
        , D.fontColor color.nodeFont
        , D.fontNameArial
        , D.labelLocC
        , D.penWidth 0.0
        ]
    , if i == 0 then
        [ SecNode $ mkNodeInit "__Start__"
        , SecEdge $ mkEdgeInit "__Start__" stateName
        ]
      else []
    , concatMap getEdge
        (getOutgoingTransitions stateName sg)
    ]
  where
  color = (getColor i).light

  getEdge = case _ of
    (R.Transition from msg [ R.Return Nothing to ]) ->
      [ SecEdge $ mkEdgeMsg from to color msg ]
    (R.Transition from msg returns) -> join
      [ pure $ SecNode $ mkDecisionNode (from <> "__" <> msg) color
      , map (mkMultiEdgeOut from msg) returns
      , pure $ SecEdge $ mkEdgeMsg from (from <> "__" <> msg) color msg
      ]

  mkMultiEdgeOut from msg = case _ of
    (R.Return Nothing to) -> SecEdge $ mkEdgeGuard (from <> "__" <> msg) to color Nothing
    _ -> unsafeCoerce "todo"

mkNodeInit :: String -> Node
mkNodeInit name = Node name
  [ D.shapeCircle
  , D.label ""
  , D.width 0.2
  , D.height 0.2
  , D.fixedSize true
  , D.styleFilled
  , D.fillColor (Color.rgb 140 140 140)
  , D.penWidth 0.0
  ]

mkEdgeMsg :: String -> String -> Colors -> String -> Edge
mkEdgeMsg from to colors label = Edge from to
  [ D.color colors.edgeColor
  , D.fontColor colors.edgeFont
  , D.fontSize 12
  , D.arrowSize 0.7
  , D.labelHtmlBold label
  , D.penWidth 1.8
  ]

mkEdgeGuard :: String -> String -> Colors -> Maybe String -> Edge
mkEdgeGuard from to colors mayLabel = Edge from to
  $ catMaybes
      [ pure $ D.color colors.edgeColor
      , pure $ D.fontColor colors.edgeFont
      , pure $ D.fontSize 12
      , pure $ D.arrowSize 0.7
      , map D.labelHtmlBold mayLabel
      , pure $ D.penWidth 1.8
      ]

mkEdgeInit :: String -> String -> Edge
mkEdgeInit from to = Edge from to
  [ D.color (Color.rgb 140 140 140)
  , D.fontSize 12
  , D.arrowSize 0.7
  , D.penWidth 1.8
  ]

mkDecisionNode :: String -> Colors -> Node
mkDecisionNode name colors = Node name
  [ D.shapeDiamond
  , D.labelHtmlBold "?"
  , D.fontSize 12
  , D.styleFilled
  , D.fontColor colors.nodeFont
  , D.fontNameArial
  ]

getOutgoingTransitions :: String -> R.StateGraph_ -> Array R.Transition_
getOutgoingTransitions stateName (R.StateGraph transitions) =
  Array.filter (\(R.Transition from _ _) -> from == stateName) transitions

getEdges :: R.StateGraph_ -> Array Section
getEdges (R.StateGraph transitions) =
  map getEdge transitions
  where
  getEdge = case _ of
    (R.Transition from msg [ R.Return Nothing to ]) -> SecEdge $ Edge from to
      [ D.labelHtmlBold msg
      --, D.fontColor (Color.rgba' 0.0 0.5 0.94 1.0)
      , D.fontSize 12
      --, D.color (Color.rgba' 0.0 0.5 0.94 1.0)
      , D.arrowSize 0.7
      ]
    _ -> unsafeCoerce "todo"

getStates :: R.StateGraph_ -> Array String
getStates sg =
  Array.nub $ Array.concat [ getFromStates sg, getToStates sg ]

getFromStates :: R.StateGraph_ -> Array String
getFromStates (R.StateGraph transitions) =
  map (\(R.Transition stateName _ _) -> stateName) transitions

getToStates :: R.StateGraph_ -> Array String
getToStates (R.StateGraph transitions) =
  join $ map
    ( \(R.Transition _ _ returns) -> map
        ( case _ of
            Return _ stateName -> stateName
        )
        returns
    )
    transitions

type Options =
  { title :: String
  , nodeLabelModifier :: String -> String
  , edgeLabelModifier :: String -> String
  , globalLabelModifier :: String -> String
  }

defaultOptions :: Options
defaultOptions =
  { title: "Untitled"
  , nodeLabelModifier: identity
  , edgeLabelModifier: identity
  , globalLabelModifier: identity
  }

writeToFile :: forall @spec. Reflectable spec R.StateGraph_ => (Options -> Options) -> FilePath -> Effect Unit
writeToFile mkOptions path = do
  let reflected = reflectType (Proxy @spec)
  Console.log $ "Reflected: " <> show reflected
  FS.writeTextFile UTF8 path
    (toText (mkGraphvizGraph (mkOptions defaultOptions) reflected))
  Console.log $ "Wrote graphviz graph to " <> path

writeToFile_ :: forall @spec. Reflectable spec R.StateGraph_ => FilePath -> Effect Unit
writeToFile_ = writeToFile @spec identity

