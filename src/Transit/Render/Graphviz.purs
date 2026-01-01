-- | Generator for Graphviz DOT language graphs from transit specifications.
-- |
-- | This module converts state machine specifications into Graphviz graphs
-- | for visualization, supporting various rendering options including
-- | decision nodes, undirected edges, and customizable themes.
module Transit.Render.Graphviz
  ( mkGraphvizGraph
  , generate
  , generate_
  , Options
  , defaultOptions
  ) where

import Prelude

import Color as Color
import Data.Array (catMaybes, concatMap, mapWithIndex)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Transit.Core (GuardName, Match(..), MsgName, Return(..), StateName, TransitCore(..), getMatchesForState, getStateNames)
import Transit.Data.DotLang (GlobalAttrs(..), GraphvizGraph(..), Section(..), toDotStr)
import Transit.Data.DotLang as D
import Transit.Render.Theme (ColorHarmony, Theme, getColorHarmony, themeHarmonyDark)
import Transit.StateGraph (StateNode)

-- | Generates a Graphviz graph from a transit specification.
mkGraphvizGraph :: Options -> TransitCore -> GraphvizGraph
mkGraphvizGraph options transit =
  GraphvizGraph $ join
    [ pure $ SecGlobal $ GlobalAttrs $ mkGlobalAttrs options
    , case options.globalAttrsRaw of
        Just raw -> [ SecGlobalRaw raw ]
        Nothing -> []
    , join $ mapWithIndex (mkStateSections transit options) $ getStateNames transit
    ]

-- | Creates sections for a single state (node, entry point edges, and transition edges).
mkStateSections :: TransitCore -> Options -> Int -> StateName -> Array D.Section
mkStateSections transit options i stateName = join
  [ pure $ SecNode $ mkStateNode options colors stateName
  , if Array.elem stateName options.entryPoints then
      [ SecNode $ mkInitNode "__Start__"
      , SecEdge $ mkInitEdge "__Start__" stateName
      ]
    else []
  , Array.concatMap (mkMatchSections colors transit options) $ getMatchesForState stateName transit
  ]
  where
  colors = getColorHarmony options.theme i

-- | Creates sections for a match (edge or decision node structure).
mkMatchSections :: ColorHarmony -> TransitCore -> Options -> Match -> Array D.Section
mkMatchSections colors transit options (Match from msg returns) = case returns of
  [ Return to ] ->
    if options.useUndirectedEdges && hasComplementaryEdge from to msg transit then
      if isCanonicalFirst from to then
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

-- | Checks if the first state name is lexicographically greater than the second.
-- | Used to determine canonical ordering for undirected edges.
isCanonicalFirst :: StateName -> StateName -> Boolean
isCanonicalFirst from to = from > to

-- | Checks if there exists a complementary edge (reverse direction with same message).
hasComplementaryEdge :: StateName -> StateName -> MsgName -> TransitCore -> Boolean
hasComplementaryEdge from to msg (TransitCore matches) =
  Array.any
    ( \(Match from' msg' returns') ->
        from' == to && msg' == msg && returns' == [ Return from ]
    )
    matches

-- | Creates direct edges from a state to multiple target states.
mkDirectEdges :: StateName -> MsgName -> ColorHarmony -> Array Return -> Array D.Section
mkDirectEdges from msg colors returns = Array.concatMap
  ( case _ of
      Return to -> [ SecEdge $ mkEdgeMsg from to colors msg ]
      ReturnVia guard to -> [ SecEdge $ mkEdgeMsg from to colors (msg <> " ? " <> guard) ]
  )
  returns

-- | Creates a decision node structure for multiple returns from a single match.
mkDecisionNodeSections :: StateName -> MsgName -> ColorHarmony -> Array Return -> Array D.Section
mkDecisionNodeSections from msg colors manyReturns =
  let
    decisionNode = "decision_" <> from <> "_" <> msg
  in
    join
      [ pure $ SecNode $ mkDecisionNode decisionNode colors
      , pure $ SecEdge $ mkEdgeMsg from decisionNode colors msg
      , concatMap (mkDecisionEdges decisionNode colors) manyReturns
      ]

-- | Creates edges from a decision node to target states.
mkDecisionEdges :: StateName -> ColorHarmony -> Return -> Array D.Section
mkDecisionEdges decisionNode colors = case _ of
  Return to -> [ SecEdge $ mkEdgeGuard decisionNode to colors Nothing ]
  ReturnVia guard to -> [ SecEdge $ mkEdgeGuard decisionNode to colors (Just guard) ]

-- | Creates global graph attributes.
mkGlobalAttrs :: Options -> Array D.Attr
mkGlobalAttrs options =
  catMaybes
    [ pure D.rankDirTD
    , pure D.fontNameArial
    , map D.labelHtmlBold options.title
    , pure D.labelLocT
    , pure $ D.fontSize 12
    , pure $ D.bgColor options.theme.bgColor
    , pure $ D.color options.theme.titleColor
    , pure $ D.fontColor options.theme.titleColor
    ]

-- | Creates a state node with styling.
mkStateNode :: Options -> ColorHarmony -> StateNode -> D.Node
mkStateNode options colors node = D.Node node (options.nodeAttrsRaw # map (\f -> f node))
  [ D.shapeBox
  , D.labelHtmlBold node
  , D.fontSize 12
  , D.styleFilled
  , D.fillColor colors.nodeBg
  , D.fontColor colors.nodeFont
  , D.color colors.nodeBorder
  , D.fontNameArial
  , D.labelLocC
  , D.penWidth 1.0
  ]

-- | Creates an initialization node (entry point marker).
mkInitNode :: String -> D.Node
mkInitNode name = D.Node name Nothing
  [ D.shapeCircle
  , D.label ""
  , D.width 0.15
  , D.height 0.15
  , D.fixedSize true
  , D.styleFilled
  , D.fillColor (Color.rgb 140 140 140)
  , D.penWidth 0.0
  ]

-- | Creates an edge from the initialization node to an entry point state.
mkInitEdge :: StateName -> StateName -> D.Edge
mkInitEdge from to = D.Edge from to
  [ D.color (Color.rgb 140 140 140)
  , D.fontSize 12
  , D.arrowSize 0.7
  , D.penWidth 1.8
  ]

-- | Creates an undirected edge (bidirectional) between two states.
mkUndirectedEdge :: StateName -> StateName -> MsgName -> D.Edge
mkUndirectedEdge from to label = D.Edge from to
  [ D.color (Color.rgb 140 140 140)
  , D.fontColor (Color.rgb 140 140 140)
  , D.fontSize 12
  , D.labelHtmlBold label
  , D.arrowSize 0.7
  , D.penWidth 2.0
  , D.dirBoth
  ]

-- | Creates a directed edge with a message label.
mkEdgeMsg :: StateName -> StateName -> ColorHarmony -> MsgName -> D.Edge
mkEdgeMsg from to colors label = D.Edge from to
  [ D.color colors.edgeColor
  , D.fontColor colors.edgeFont
  , D.fontSize 12
  , D.arrowSize 0.7
  , D.labelHtmlBold label
  , D.penWidth 1.8
  ]

-- | Creates an edge from a decision node to a target state, optionally with a guard label.
mkEdgeGuard :: StateName -> StateName -> ColorHarmony -> Maybe GuardName -> D.Edge
mkEdgeGuard from to colors mayLabel = D.Edge from to
  $ catMaybes
      [ pure $ D.color colors.edgeColor
      , pure $ D.fontColor colors.edgeFont
      , pure $ D.fontSize 10
      , pure $ D.arrowSize 0.5
      , map D.labelHtmlItalic mayLabel
      , pure $ D.penWidth 1.0
      ]

-- | Creates a decision node (diamond shape) for branching transitions.
mkDecisionNode :: String -> ColorHarmony -> D.Node
mkDecisionNode name colors = D.Node name Nothing
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

-- | Configuration options for graph generation.
type Options =
  { title :: Maybe String
  , theme :: Theme
  , globalAttrsRaw :: Maybe String
  , nodeAttrsRaw :: Maybe (StateName -> String)
  , useDecisionNodes :: Boolean
  , useUndirectedEdges :: Boolean
  , entryPoints :: Array StateName
  }

-- | Default options for graph generation.
defaultOptions :: Options
defaultOptions =
  { title: Nothing
  , theme: themeHarmonyDark
  , globalAttrsRaw: Nothing
  , nodeAttrsRaw: Nothing
  , useDecisionNodes: true
  , useUndirectedEdges: false
  , entryPoints: []
  }

-- | Generates a Graphviz graph as a string with customizable options.
generate :: TransitCore -> (Options -> Options) -> String
generate transitCore mkOptions =
  toDotStr (mkGraphvizGraph (mkOptions defaultOptions) transitCore)

-- | Generates a Graphviz graph as a string with default options.
generate_ :: TransitCore -> String
generate_ transitCore = generate transitCore identity

