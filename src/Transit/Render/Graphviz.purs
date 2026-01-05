-- | Generator for Graphviz DOT language graphs from transit specifications.
-- |
-- | This module converts state machine specifications into Graphviz graphs
-- | for visualization, supporting various rendering options including
-- | decision nodes, undirected edges, and customizable themes.
module Transit.Render.Graphviz
  ( NodePosition
  , Options
  , Layout(..)
  , defaultOptions
  , generate
  , generateEither
  , mkGraphvizGraph
  ) where

import Prelude

import Color as Color
import Data.Array (catMaybes, concatMap, mapWithIndex)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), maybe)
import Transit.Core (GuardName, Match(..), MsgName, Return(..), StateName, TransitCore(..), getMatchesForState, getStateNames)
import Transit.Data.DotLang (GlobalAttrs(..), GraphvizGraph(..), Section(..))
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
  join
    [ [ D.rankDirTD
      , D.fontNameArial
      , D.labelLocT
      , D.fontSize 12
      , D.bgColor options.theme.bgColor
      , D.color options.theme.titleColor
      , D.fontColor options.theme.titleColor
      , D.pad 0.2
      ]
    , maybe [] (pure <<< D.labelHtmlBold) options.title
    , case options.layout of
        Landscape -> [ D.layoutDot, D.rankDirLR ]
        Portrait -> [ D.layoutDot, D.rankDirTD ]
        Circle -> [ D.layoutCirco ]
        Manual _ -> [ D.layoutNeato ]
        None -> []
    ]

-- | Creates a state node with styling.
mkStateNode :: Options -> ColorHarmony -> StateNode -> D.Node
mkStateNode options colors node = D.Node node (options.nodeAttrsRaw # map (\f -> f node))
  $ join
      [ [ D.shapeBox
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
      , case options.layout of
          Manual positions ->
            case positions # Array.find (\position -> position.node == node) of
              Just position -> [ D.pos position.x position.y position.exact ]
              Nothing -> []
          _ -> []
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
  , layout :: Layout
  }

type NodePosition =
  { node :: String
  , x :: Number
  , y :: Number
  , exact :: Boolean
  }

data Layout
  = Landscape
  | Portrait
  | Manual (Array NodePosition)
  | Circle
  | None

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
  , layout: Portrait
  }

checkEntryPoints :: Array StateName -> TransitCore -> Either String Unit
checkEntryPoints entryPoints transitCore = do
  for_ entryPoints \entryPoint -> do
    if entryPoint `Array.elem` getStateNames transitCore then
      pure unit
    else
      Left $ "Entry point " <> entryPoint <> " not found in transit core"

checkPositions :: Array NodePosition -> TransitCore -> Either String Unit
checkPositions positions transitCore = do
  for_ positions \position -> do
    if position.node `Array.elem` getStateNames transitCore then
      pure unit
    else
      Left $ "Node " <> position.node <> " not found in transit core"

checkOptions :: Options -> TransitCore -> Either String Unit
checkOptions options transitCore = do
  case options.layout of
    Manual positions -> checkPositions positions transitCore
    _ -> pure unit
  checkEntryPoints options.entryPoints transitCore

-- | Generates a Graphviz graph with customizable options. Fails if the options are invalid.
generateEither :: TransitCore -> (Options -> Options) -> Either String GraphvizGraph
generateEither transitCore mkOptions = do
  checkOptions (mkOptions defaultOptions) transitCore
  pure $ mkGraphvizGraph (mkOptions defaultOptions) transitCore

-- | Generates a Graphviz graph with customizable options.
generate :: TransitCore -> (Options -> Options) -> GraphvizGraph
generate transitCore mkOptions =
  mkGraphvizGraph (mkOptions defaultOptions) transitCore
