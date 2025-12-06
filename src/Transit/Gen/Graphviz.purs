module Transit.Gen.Graphviz where

import Prelude

import Color (Color, ColorSpace(..))
import Color as Color
import Data.Array (catMaybes, concatMap, mapWithIndex, (!!))
import Data.Array as Array
import Data.Map (Map)
import Data.Map as Map
import Data.Map.Internal (unsafeBalancedNode)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Reflectable (class Reflectable, reflectType)
import Data.Set as Set
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Class.Console as Console
import Node.Encoding (Encoding(..))
import Node.FS.Sync as FS
import Node.Path (FilePath)
import Transit.Colors (Colors, defLight, getColor)
import Transit.Colors as Colors
import Transit.DotLang (GlobalAttrs(..), GraphvizGraph(..), Section(..), rankDirTD, toText)
import Transit.DotLang as D
import Transit.Graph (NodeGroup, EdgeGroup)
import Transit.Graph as Graph
import Transit.StateGraph (Edge, StateGraph, Node)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

mkGraphvizGraph :: Options -> StateGraph -> GraphvizGraph
mkGraphvizGraph options sg = GraphvizGraph $ join
  [ pure $ SecGlobal $ GlobalAttrs $ mkGlobalAttrs options
  , join $ mapWithIndex (f sg colorMap) $ Set.toUnfoldable $ Graph.getGrouped sg
  ]
  where
  colorMap = mkColorMap sg

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

mkColorMap :: StateGraph -> ColorMap
mkColorMap sg = Map.fromFoldable $ mapWithIndex (\i node -> (node.state /\ (getColor i).light)) $ Set.toUnfoldable $ Graph.getNodes sg

f :: StateGraph -> ColorMap -> Int -> NodeGroup Edge Node -> Array Section
f sg colorMap i { fromNode, edges } = join
  [ pure $ SecNode $ mkStateNode colors fromNode
  , if i == 0 then
      [ SecNode $ mkInitNode "__Start__"
      , SecEdge $ mkInitEdge "__Start__" fromNode.state
      ]
    else []
  , concatMap (g sg colorMap fromNode) $ Set.toUnfoldable edges
  ]
  where
  colors = lookupColor fromNode.state colorMap

mkStateNode :: Colors -> Node -> D.Node
mkStateNode colors node = D.Node node.state
  [ D.shapeBox
  , D.labelHtmlBold node.state
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

g :: StateGraph -> ColorMap -> Node -> EdgeGroup Edge Node -> Array Section
g sg colorMap fromNode { edge, toNodes } = case Set.toUnfoldable toNodes of
  [ toNode ] ->
    if (Graph.hasEdge { fromNode: toNode, edge: edge, toNode: fromNode } sg) then
      ( if fromNode > toNode then
          let
            colorsTo = lookupColor toNode.state colorMap
          in
            [ SecEdge $ mkEdgeMsg fromNode.state toNode.state (defLight Color.black) edge.msg
            ]
        else []
      )
    else [ SecEdge $ mkEdgeMsg fromNode.state toNode.state colorsFrom edge.msg ]
  _ -> join
    [ pure $ SecNode $ mkDecisionNode fromNode.state colorsFrom
    , concatMap (\toNode -> [ SecEdge $ mkEdgeMsg fromNode.state toNode.state colorsFrom edge.msg ]) $ Set.toUnfoldable toNodes
    ]

  where
  colorsFrom = lookupColor fromNode.state colorMap

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

-- where
-- getStates sg = Array.nub $ Array.concat [ getFromStates sg, getToStates sg ]
-- getFromStates sg = Array.map (\(R.Transition stateName _ _) -> stateName) transitions
-- getToStates sg = Array.map (\(R.Transition _ _ returns) -> Array.concatMap (\(R.Return stateName) -> [ stateName ]) returns) transitions

-- mkGraphvizGraph :: Options -> R.StateGraph_ -> GraphvizGraph
-- mkGraphvizGraph options sg@(R.StateGraph transitions) = GraphvizGraph $ join
--   [ pure $ SecGlobal $
--       D.GlobalAttrs
--         [ D.rankDirTD
--         , D.fontNameArial
--         , D.labelHtmlBold options.title
--         , D.labelLocT
--         , D.fontSize 12
--         ]
--   , join $ mapWithIndex (mkNode sg) (getStates sg)
--   ]

-- mkNode :: R.StateGraph_ -> Int -> String -> Array Section
-- mkNode sg i stateName =
--   join
--     [ pure $ SecNode $ Node stateName
--         [ D.shapeBox
--         , D.labelHtmlBold stateName
--         , D.fontSize 12
--         , D.styleFilled
--         , D.fillColor color.nodeBg
--         , D.fontColor color.nodeFont
--         , D.fontNameArial
--         , D.labelLocC
--         , D.penWidth 0.0
--         ]
--     , if i == 0 then
--         [ SecNode $ mkNodeInit "__Start__"
--         , SecEdge $ mkEdgeInit "__Start__" stateName
--         ]
--       else []
--     , concatMap getEdge
--         (getOutgoingTransitions stateName sg)
--     ]
--   where
--   color = (getColor i).light

--   getEdge = case _ of
--     (R.Transition from msg [ R.Return to ]) ->
--       [ SecEdge $ mkEdgeMsg from to color msg ]
--     (R.Transition from msg returns) -> join
--       [ pure $ SecNode $ mkDecisionNode (from <> "__" <> msg) color
--       , map (mkMultiEdgeOut from msg) returns
--       , pure $ SecEdge $ mkEdgeMsg from (from <> "__" <> msg) color msg
--       ]

--   mkMultiEdgeOut from msg = case _ of
--     (R.Return to) -> SecEdge $ mkEdgeGuard (from <> "__" <> msg) to color Nothing
--     _ -> unsafeCoerce "todo"

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
      , pure $ D.fontSize 12
      , pure $ D.arrowSize 0.7
      , map D.labelHtmlBold mayLabel
      , pure $ D.penWidth 1.8
      ]

mkDecisionNode :: String -> Colors -> D.Node
mkDecisionNode name colors = D.Node name
  [ D.shapeDiamond
  , D.labelHtmlBold "?"
  , D.fontSize 12
  , D.styleFilled
  , D.fontColor colors.nodeFont
  , D.fontNameArial
  ]

-- getOutgoingTransitions :: String -> R.StateGraph_ -> Array R.Transition_
-- getOutgoingTransitions stateName (R.StateGraph transitions) =
--   Array.filter (\(R.Transition from _ _) -> from == stateName) transitions

-- getEdges :: R.StateGraph_ -> Array Section
-- getEdges (R.StateGraph transitions) =
--   map getEdge transitions
--   where
--   getEdge = case _ of
--     (R.Transition from msg [ R.Return to ]) -> SecEdge $ Edge from to
--       [ D.labelHtmlBold msg
--       --, D.fontColor (Color.rgba' 0.0 0.5 0.94 1.0)
--       , D.fontSize 12
--       --, D.color (Color.rgba' 0.0 0.5 0.94 1.0)
--       , D.arrowSize 0.7
--       ]
--     _ -> unsafeCoerce "todo"

-- getStates :: R.StateGraph_ -> Array String
-- getStates sg =
--   Array.nub $ Array.concat [ getFromStates sg, getToStates sg ]

-- getFromStates :: R.StateGraph_ -> Array String
-- getFromStates (R.StateGraph transitions) =
--   map (\(R.Transition stateName _ _) -> stateName) transitions

-- getToStates :: R.StateGraph_ -> Array String
-- getToStates (R.StateGraph transitions) =
--   join $ map
--     ( \(R.Transition _ _ returns) -> map
--         ( case _ of
--             Return stateName -> stateName
--             ReturnVia _ stateName -> stateName
--         )
--         returns
--     )
--     transitions

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

writeToFile :: StateGraph -> (Options -> Options) -> FilePath -> Effect Unit
writeToFile sg mkOptions path = do
  FS.writeTextFile UTF8 path
    (toText (mkGraphvizGraph (mkOptions defaultOptions) sg))
  Console.log $ "Wrote graphviz graph to " <> path

writeToFile_ :: StateGraph -> FilePath -> Effect Unit
writeToFile_ sg path = writeToFile sg identity path

