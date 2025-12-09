module Transit.Generators.Graphviz
  ( mkGraphvizGraph
  , Options
  , defaultOptions
  , writeToFile
  , writeToFile_
  ) where

import Prelude

import Color (ColorSpace(..))
import Color as Color
import Data.Array (catMaybes, concatMap, mapWithIndex)
import Data.Array as Array
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Class.Console as Console
import Node.Encoding (Encoding(..))
import Node.FS.Sync as FS
import Node.Path (FilePath)
import Transit.Colors (Colors, getColor)
import Transit.Colors as Colors
import Transit.Core (Match(..), Return(..), TransitCore(..), getMatchesForState, getStateNames)
import Transit.Data.DotLang (GlobalAttrs(..), GraphvizGraph(..), Section(..), toText)
import Transit.Data.DotLang as D
import Transit.StateGraph (Node)

mkGraphvizGraph :: Options -> TransitCore -> GraphvizGraph
mkGraphvizGraph options transit =
  GraphvizGraph $ join
    [ pure $ SecGlobal $ GlobalAttrs $ mkGlobalAttrs options
    , case options.globalAttrsRaw of
        Just raw -> [ SecGlobalRaw raw ]
        Nothing -> []
    , join $ mapWithIndex (mkStateSections colorMap transit options) $ getStateNames transit
    ]
  where
  colorMap = mkColorMap transit

mkStateSections :: ColorMap -> TransitCore -> Options -> Int -> String -> Array D.Section
mkStateSections colorMap transit options i stateName = join
  [ pure $ SecNode $ mkStateNode colors stateName
  , if i == 0 then
      [ SecNode $ mkInitNode "__Start__"
      , SecEdge $ mkInitEdge "__Start__" stateName
      ]
    else []
  , Array.concatMap (mkMatchSections colors transit options) $ getMatchesForState stateName transit
  ]
  where
  colors = lookupColor stateName colorMap

mkMatchSections :: Colors -> TransitCore -> Options -> Match -> Array D.Section
mkMatchSections colors transit options (Match from msg returns) = case returns of
  [ Return to ] ->
    if options.useUndirectedEdges then
      if (drawUnidirectionalEdge from to msg transit) then
        [ SecEdge $ mkUndirectedEdge from to msg ]
      else
        []
    else
      [ SecEdge $ mkEdgeMsg from to colors msg ]
  manyReturns ->
    if options.useDecisionNodes then
      mkDecisionNodeSections from msg colors manyReturns
    else
      mkDirectEdges from msg colors manyReturns

drawUnidirectionalEdge :: String -> String -> String -> TransitCore -> Boolean
drawUnidirectionalEdge from to msg (TransitCore matches) = (from > to) &&
  ( Array.any (\(Match from' msg' returns') -> from' == to && msg' == msg && returns' == [ Return from ]) matches
  )

mkDirectEdges :: String -> String -> Colors -> Array Return -> Array D.Section
mkDirectEdges from msg colors returns = Array.concatMap
  ( case _ of
      Return to -> [ SecEdge $ mkEdgeMsg from to colors msg ]
      ReturnVia guard to -> [ SecEdge $ mkEdgeMsg from to colors (msg <> " ? " <> guard) ]
  )
  returns

mkDecisionNodeSections :: String -> String -> Colors -> Array Return -> Array D.Section
mkDecisionNodeSections from msg colors manyReturns =
  let
    decisionNode = "decision_" <> from <> "_" <> msg
  in
    join
      [ pure $ SecNode $ mkDecisionNode decisionNode colors
      , pure $ SecEdge $ mkEdgeMsg from decisionNode colors msg
      , concatMap (mkDecisionEdges decisionNode colors) manyReturns
      ]

mkDecisionEdges :: String -> Colors -> Return -> Array D.Section
mkDecisionEdges decisionNode colors = case _ of
  Return to -> [ SecEdge $ mkEdgeGuard decisionNode to colors Nothing ]
  ReturnVia guard to -> [ SecEdge $ mkEdgeGuard decisionNode to colors (Just guard) ]

mkGlobalAttrs :: Options -> Array D.Attr
mkGlobalAttrs options =
  [ D.rankDirTD
  , D.fontNameArial
  , D.labelHtmlBold options.title
  , D.labelLocT
  , D.fontSize 12
  ]

type ColorMap = Map String Colors

lookupColor :: String -> ColorMap -> Colors
lookupColor state colorMap = fromMaybe (Colors.defLight Color.black) $ Map.lookup state colorMap

mkColorMap :: TransitCore -> ColorMap
mkColorMap transit =
  Map.fromFoldable $ mapWithIndex (\i node -> (node /\ (getColor i).light)) $ getStateNames transit

mkStateNode :: Colors -> Node -> D.Node
mkStateNode colors node = D.Node node
  [ D.shapeBox
  , D.labelHtmlBold node
  , D.fontSize 12
  , D.styleFilled
  , D.fillColor colors.nodeBg
  , D.fontColor colors.nodeFont
  , D.fontNameArial
  , D.labelLocC
  , D.penWidth 0.0
  ]

mkInitNode :: String -> D.Node
mkInitNode name = D.Node name
  [ D.shapeCircle
  , D.label ""
  , D.width 0.15
  , D.height 0.15
  , D.fixedSize true
  , D.styleFilled
  , D.fillColor (Color.rgb 140 140 140)
  , D.penWidth 0.0
  ]

mkInitEdge :: String -> String -> D.Edge
mkInitEdge from to = D.Edge from to
  [ D.color (Color.rgb 140 140 140)
  , D.fontSize 12
  , D.arrowSize 0.7
  , D.penWidth 1.8
  ]

mkUndirectedEdge :: String -> String -> String -> D.Edge
mkUndirectedEdge from to label = D.Edge from to
  [ D.color (Color.rgb 140 140 140)
  , D.fontColor (Color.rgb 140 140 140)
  , D.fontSize 12
  , D.labelHtmlBold label
  , D.arrowSize 1.5
  , D.penWidth 2.0
  , D.dirBoth
  , D.arrowHeadNone
  , D.arrowTailNone
  ]

mkEdgeMsg :: String -> String -> Colors -> String -> D.Edge
mkEdgeMsg from to colors label = D.Edge from to
  [ D.color colors.edgeColor
  , D.fontColor colors.edgeFont
  , D.fontSize 12
  , D.arrowSize 0.7
  , D.labelHtmlBold label
  , D.penWidth 1.8
  ]

mkEdgeGuard :: String -> String -> Colors -> Maybe String -> D.Edge
mkEdgeGuard from to colors mayLabel = D.Edge from to
  $ catMaybes
      [ pure $ D.color colors.edgeColor
      , pure $ D.fontColor colors.edgeFont
      , pure $ D.fontSize 10
      , pure $ D.arrowSize 0.5
      , map D.labelHtmlItalic mayLabel
      , pure $ D.penWidth 1.8
      , pure $ D.penWidth 1.0
      ]

mkDecisionNode :: String -> Colors -> D.Node
mkDecisionNode name colors = D.Node name
  [ D.shapeDiamond
  , D.label "?"
  , D.fontSize 12
  , D.fontColor colors.nodeFont
  , D.styleFilled
  , D.fillColor colors.nodeBg
  , D.penWidth 0.0
  , D.fixedSize true
  , D.width 0.3
  , D.height 0.3
  ]

type Options =
  { title :: String
  , globalAttrsRaw :: Maybe String
  , useDecisionNodes :: Boolean
  , useUndirectedEdges :: Boolean
  }

defaultOptions :: Options
defaultOptions =
  { title: "Untitled"
  , globalAttrsRaw: Nothing
  , useDecisionNodes: true
  , useUndirectedEdges: false
  }

writeToFile :: (Options -> Options) -> TransitCore -> FilePath -> Effect Unit
writeToFile mkOptions sg path = do
  FS.writeTextFile UTF8 path
    (toText (mkGraphvizGraph (mkOptions defaultOptions) sg))
  Console.log $ "Wrote graphviz graph to " <> path

writeToFile_ :: TransitCore -> FilePath -> Effect Unit
writeToFile_ sg path = writeToFile identity sg path

