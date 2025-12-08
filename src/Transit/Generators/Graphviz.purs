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
import Data.Set as Set
import Data.Tuple.Nested ((/\))
import Debug (spy, spyWith)
import Effect (Effect)
import Effect.Class.Console as Console
import Node.Encoding (Encoding(..))
import Node.FS.Sync as FS
import Node.Path (FilePath)
import Transit.Colors (Colors, defLight, getColor)
import Transit.Colors as Colors
import Transit.Core (Match_(..), MsgName_, Return, Return_(..), StateName_, TransitCore, TransitCore_, getMatchesForState, getStateNames)
import Transit.Data.DotLang (GlobalAttrs(..), GraphvizGraph(..), Section(..), toText)
import Transit.Data.DotLang as D
import Transit.Data.Graph as Graph
import Transit.StateGraph (Edge, Node, StateGraph(..))
import Unsafe.Coerce (unsafeCoerce)

mkGraphvizGraph :: Options -> TransitCore_ -> GraphvizGraph
mkGraphvizGraph options transit =
  GraphvizGraph $ join
    [ pure $ SecGlobal $ GlobalAttrs $ mkGlobalAttrs options
    , case options.globalAttrsRaw of
        Just raw -> [ SecGlobalRaw raw ]
        Nothing -> []
    --, join $ mapWithIndex (mkNode sg colorMap) $ spyWith "nodes" show $ Set.toUnfoldable $ Graph.getGrouped g
    , join $ mapWithIndex (h colorMap transit) $ getStateNames transit
    ]
  where
  colorMap = mkColorMap transit

h :: ColorMap -> TransitCore_ -> Int -> StateName_ -> Array D.Section
h colorMap transit i stateName = join
  [ pure $ SecNode $ mkStateNode colors stateName
  , if i == 0 then
      [ SecNode $ mkInitNode "__Start__"
      , SecEdge $ mkInitEdge "__Start__" stateName
      ]
    else []
  , Array.concatMap (f colors) $ getMatchesForState stateName transit
  ]
  where
  colors = lookupColor stateName colorMap

f :: Colors -> Match_ -> Array D.Section
f colors (Match from msg returns) = case returns of
  [ Return to ] -> [ SecEdge $ mkEdgeMsg from to colors msg ]
  _ -> []

g :: StateName_ -> MsgName_ -> Return_ -> Array D.Section
g _ _ _ = []

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

mkColorMap :: TransitCore_ -> ColorMap
mkColorMap transit =
  Map.fromFoldable $ mapWithIndex (\i node -> (node /\ (getColor i).light)) $ getStateNames transit

-- mkNode :: StateGraph -> ColorMap -> Int -> NodeInfo Edge Node -> Array Section
-- mkNode sg@(StateGraph meta g) colorMap i { fromNode, edges } = join
--   [ pure $ SecNode $ mkStateNode colors fromNode
--   , if Array.elem fromNode meta.entryPoints then
--       [ SecNode $ mkInitNode "__Start__"
--       , SecEdge $ mkInitEdge "__Start__" fromNode
--       ]
--     else []
--   --  , concatMap (mkEdge sg colorMap fromNode) $ Set.toUnfoldable edges
--   ]
--   where
--   colors = lookupColor fromNode colorMap

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

-- mkEdge :: StateGraph -> ColorMap -> Node -> EdgeInfo Edge Node -> Array Section
-- mkEdge (StateGraph _ sg) colorMap fromNode { edge, toNode } = case Set.toUnfoldable toNodes of
--   [ toNode ] ->
--     if (Graph.hasEdge { fromNode: toNode, edge: edge, toNode: fromNode } sg) then
--       ( if fromNode > toNode then
--           [ --SecEdge $ mkEdgeMsg fromNode toNode (defLight Color.black) edge.msg
--           ]
--         else []
--       )
--     else [ SecEdge $ mkEdgeMsg fromNode toNode colorsFrom edge.msg ]
--   manyNodes ->
--     let
--       decisionNode = "decision_" <> fromNode <> "_" <> edge.msg
--     in
--       join
--         [ pure $ SecNode $ mkDecisionNode decisionNode colorsFrom
--         , pure $ SecEdge $ mkEdgeMsg fromNode decisionNode colorsFrom edge.msg
--         , concatMap (\toNode -> [ SecEdge $ mkEdgeGuard decisionNode toNode colorsFrom Nothing ]) manyNodes
--         ]

--   where
--   colorsFrom = lookupColor fromNode colorMap

mkUndirectedEdge :: String -> String -> Colors -> Colors -> String -> D.Edge
mkUndirectedEdge from to fromColors toColors label = D.Edge from to
  [ D.color (Color.mix RGB fromColors.edgeFont toColors.edgeFont 0.5)
  , D.fontColor (Color.mix RGB fromColors.edgeFont toColors.edgeFont 0.5)
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
  }

defaultOptions :: Options
defaultOptions =
  { title: "Untitled"
  , globalAttrsRaw: Nothing
  }

writeToFile :: (Options -> Options) -> TransitCore_ -> FilePath -> Effect Unit
writeToFile mkOptions sg path = do
  FS.writeTextFile UTF8 path
    (toText (mkGraphvizGraph (mkOptions defaultOptions) sg))
  Console.log $ "Wrote graphviz graph to " <> path

writeToFile_ :: TransitCore_ -> FilePath -> Effect Unit
writeToFile_ sg path = writeToFile identity sg path

