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
import Transit.Core (Return_(..), StateGraph_(..), Transition_(..))
import Transit.DotLang (Edge(..), GraphvizGraph(..), Node(..), Section(..), rankDirTD, toText)
import Transit.DotLang as D
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

colors :: Array { light :: Color, dark :: Color }
colors = map
  ( \(light /\ dark) ->
      { light: fromMaybe Color.black (Color.fromHexString light)
      , dark: fromMaybe Color.black (Color.fromHexString dark)
      }
  )
  [ "#2F80ED" /\ "#3A78E8"
  , "#F2994A" /\ "#DB8D44"
  , "#EB5757" /\ "#D95757"
  , "#56CCF2" /\ "#52C4E8"
  , "#A3E635" /\ "#91E134"
  , "#9B51E0" /\ "#9C51E3"
  , "#F2C94C" /\ "#DEBA3E"
  , "#2DC653" /\ "#31BB5D"
  , "#FF6B81" /\ "#E7677D"
  , "#3A82F7" /\ "#467AF2"
  , "#F66B0E" /\ "#E8751C"
  , "#6FCF97" /\ "#66C88D"
  , "#BB6BD9" /\ "#B268D6"
  , "#2D9CDB" /\ "#359FDF"
  , "#FF8FA3" /\ "#E88797"
  , "#4C6EF5" /\ "#587AF7"
  , "#FFC300" /\ "#E9B300"
  , "#C471F5" /\ "#B66EF3"
  , "#27AE60" /\ "#2AB76A"
  , "#FF4D6D" /\ "#E54865"
  , "#8B5CF6" /\ "#7C4DF2"
  , "#FF8B3D" /\ "#E7823A"
  , "#56CCF2" /\ "#52C4E8"
  ]

getColor :: Int -> { light :: Color, dark :: Color }
getColor i = fromMaybe { light: Color.black, dark: Color.black } (colors !! i `mod` Array.length colors)

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
        , D.fillColor (getColor i).light
        , D.fontColor (Color.darken 0.2 (getColor i).dark)
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
  getEdge = case _ of
    (Transition from msg [ Return to ]) -> SecEdge $ Edge from to
      [ D.labelHtmlBold msg
      , D.fontColor (Color.darken 0.2 (getColor i).dark)
      , D.color (getColor i).light
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

