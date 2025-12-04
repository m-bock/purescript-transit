module Transit.Gen.Graphviz where

import Prelude

import Color (Color)
import Color as Color
import Data.Array (catMaybes, mapWithIndex, (!!))
import Data.Array as Array
import Data.Maybe (fromMaybe)
import Data.Reflectable (class Reflectable, reflectType)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Class.Console as Console
import Node.Encoding (Encoding(..))
import Node.FS.Sync as FS
import Node.Path (FilePath)
import Transit.Colors (getColor)
import Transit.Core (Return_(..), StateGraph_(..), Transition_(..))
import Transit.DotLang (Edge(..), GraphvizGraph(..), Node(..), Section(..), rankDirTD, toText)
import Transit.DotLang as D
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

mkGraphvizGraph :: Options -> StateGraph_ -> GraphvizGraph
mkGraphvizGraph options sg@(StateGraph transitions) = GraphvizGraph $ join
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

mkNode :: StateGraph_ -> Int -> String -> Array Section
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
        [ SecNode $ Node "__Start__"
            [ D.shapeCircle
            , D.label ""
            , D.width 0.2
            , D.height 0.2
            , D.fixedSize true
            , D.styleFilled
            --  , D.fillColor (Color.rgba' 0.0 0.0 0.5 1.0)
            , D.penWidth 0.0
            ]
        , SecEdge $ Edge "__Start__" stateName
            [ D.arrowSize 0.7
            ]
        ]
      else []
    , map getEdge
        (getOutgoingTransitions stateName sg)
    ]
  where
  color = (getColor i).light

  getEdge = case _ of
    (Transition from msg [ Return to ]) -> SecEdge $ Edge from to
      [ D.labelHtmlBold msg
      , D.fontColor color.edgeFont
      , D.color color.edgeColor
      , D.fontSize 12
      , D.arrowSize 0.7
      ]
    _ -> unsafeCoerce "todo"

getOutgoingTransitions :: String -> StateGraph_ -> Array Transition_
getOutgoingTransitions stateName (StateGraph transitions) =
  Array.filter (\(Transition from _ _) -> from == stateName) transitions

getEdges :: StateGraph_ -> Array Section
getEdges (StateGraph transitions) =
  map getEdge transitions
  where
  getEdge = case _ of
    (Transition from msg [ Return to ]) -> SecEdge $ Edge from to
      [ D.labelHtmlBold msg
      --, D.fontColor (Color.rgba' 0.0 0.5 0.94 1.0)
      , D.fontSize 12
      --, D.color (Color.rgba' 0.0 0.5 0.94 1.0)
      , D.arrowSize 0.7
      ]
    _ -> unsafeCoerce "todo"

getStates :: StateGraph_ -> Array String
getStates sg =
  Array.nub $ Array.concat [ getFromStates sg, getToStates sg ]

getFromStates :: StateGraph_ -> Array String
getFromStates (StateGraph transitions) =
  map (\(Transition stateName _ _) -> stateName) transitions

getToStates :: StateGraph_ -> Array String
getToStates (StateGraph transitions) =
  join $ map
    ( \(Transition _ _ returns) -> map
        ( case _ of
            Return stateName -> stateName
            ReturnVia _ stateName -> stateName
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

writeToFile :: forall @spec. Reflectable spec StateGraph_ => (Options -> Options) -> FilePath -> Effect Unit
writeToFile mkOptions path = do
  let reflected = reflectType (Proxy @spec)
  Console.log $ "Reflected: " <> show reflected
  FS.writeTextFile UTF8 path
    (toText (mkGraphvizGraph (mkOptions defaultOptions) reflected))
  Console.log $ "Wrote graphviz graph to " <> path

writeToFile_ :: forall @spec. Reflectable spec StateGraph_ => FilePath -> Effect Unit
writeToFile_ = writeToFile @spec identity

