-- | Generator for Graphviz DOT language graphs from transit specifications.
-- |
-- | This module converts state machine specifications into Graphviz graphs
-- | for visualization, supporting various rendering options including
-- | decision nodes, undirected edges, and customizable themes.
module Transit.Render.Graphviz
  ( Inch(..)
  , Layout(..)
  , NodePositioning
  , Options
  , Vec
  , defaultOptions
  , generate
  , generateEither
  , mkGraphvizGraph
  ) where

import Prelude

import Data.Array (catMaybes, concatMap, filter, mapWithIndex)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (fold, for_)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, unwrap)
import Transit.Core (GuardName, Match(..), MsgName, Return(..), StateName, TransitCore(..), getMatchesForState, getStateNames)
import Transit.Data.DotLang (GraphvizGraph(..), Section(..))
import Transit.Data.DotLang as D
import Transit.Render.Theme (ColorHarmony, Theme, getColorHarmony, themeHarmonyDark)
import Transit.StateGraph (StateNode)

-- | 2D vector type for node positioning in inches.
type Vec = { x :: Inch, y :: Inch }

-- | Specifies manual positioning for a node in the graph.
type NodePositioning =
  { node :: String
  , pos :: Vec
  , exact :: Boolean
  }

-- | Graph layout algorithm to use for arranging nodes.
data Layout
  = Landscape -- Left-to-right horizontal layout
  | Portrait -- Top-to-bottom vertical layout
  | Manual (Array NodePositioning) -- Manual positioning with explicit coordinates
  | Circle -- Circular layout
  | None -- No automatic layout

-- | Measurement unit for node positions and sizes (inches).
newtype Inch = Inch Number

derive instance Newtype Inch _

-- | Configuration options for graph generation.
type Options =
  { title :: Maybe String
  , theme :: Theme
  , decisionNodes :: Boolean
  , undirectedEdges :: Boolean
  , entryPoints :: Array StateName
  , layout :: Layout
  , fixedNodeSize :: Maybe Vec
  , fontSize :: Number
  , extraGraphAttrs :: Array D.Attr
  , extraNodeAttrs :: Array D.Attr
  , extraEdgeAttrs :: Array D.Attr
  }

-- | Default options for graph generation.
defaultOptions :: Options
defaultOptions =
  { title: Nothing
  , theme: themeHarmonyDark
  , decisionNodes: true
  , undirectedEdges: false
  , entryPoints: []
  , layout: Portrait
  , fixedNodeSize: Nothing
  , fontSize: 12.0
  , extraGraphAttrs: []
  , extraNodeAttrs: []
  , extraEdgeAttrs: []
  }

-- | Internal constants for graph rendering (arrow sizes, pen widths, etc.).
type Constants =
  { arrowSize :: Number
  , edgePenWidth :: Number
  , nodePenWidth :: Number
  , nodeDefaultHeight :: Number
  , initNodeSize :: Number
  }

-- | Default rendering constants used throughout the graph generation.
constants :: Constants
constants =
  { arrowSize: 0.7
  , edgePenWidth: 1.8
  , nodePenWidth: 0.0
  , nodeDefaultHeight: 0.4
  , initNodeSize: 0.15
  }

mkInitNodeName :: StateName -> String
mkInitNodeName name = "__ENTRY__" <> name

mkDecisionNodeName :: StateName -> MsgName -> String
mkDecisionNodeName name msg = "__DECISION__" <> name <> "__" <> msg

-- | Generates a Graphviz graph from a transit specification.
mkGraphvizGraph :: Options -> TransitCore -> GraphvizGraph
mkGraphvizGraph options transit =
  GraphvizGraph $ join $ Array.intersperse [ SecNewline ] $ filter (not <<< Array.null)
    [ mkGroup "Global attributes"
        [ SecGlobalGraph $ mkGlobalGraphAttrs options
        , SecGlobalNode $ mkGlobalNodeAttrs options
        , SecGlobalEdge $ mkGlobalEdgeAttrs options
        ]
    , mkGroup "Entry points" initNodeSections
    , mkGroup "Nodes" (concatMap (\section -> map SecNode section.nodes) sections)
    , mkGroup "Edges" (concatMap (\section -> map SecEdge section.edges) sections)
    ]

  where
  mkGroup :: String -> Array D.Section -> Array D.Section
  mkGroup name items = case items of
    [] -> []
    someItems -> [ SecCommentLine name ] <> someItems

  sections = mapWithIndex (mkStateSections transit options) $ getStateNames transit
  initNodeSections = map SecNode $ map (mkInitNode options) options.entryPoints

-- | Creates sections for a single state (node and transition edges).
mkStateSections :: TransitCore -> Options -> Int -> StateName -> { nodes :: Array D.Node, edges :: Array D.Edge }
mkStateSections transit options i stateName =
  fold
    ( [ { nodes: [ mkStateNode options colors stateName ]
        , edges:
            if stateName `Array.elem` options.entryPoints then [ mkInitEdge options (mkInitNodeName stateName) stateName ]
            else []
        }
      ] <> (map (mkMatchSections colors transit options) $ getMatchesForState stateName transit)
    )
  where
  colors = getColorHarmony options.theme i

-- | Creates sections for a match (edge or decision node structure).
mkMatchSections :: ColorHarmony -> TransitCore -> Options -> Match -> { nodes :: Array D.Node, edges :: Array D.Edge }
mkMatchSections colors transit options (Match from msg returns) = case returns of
  [ Return to ] ->
    if options.undirectedEdges && hasComplementaryEdge from to msg transit then
      if isCanonicalFirst from to then
        { nodes: [], edges: [ mkUndirectedEdge options from to msg ] }
      else
        { nodes: [], edges: [] }
    else
      { nodes: [], edges: [ mkEdgeMsg from to colors msg ] }
  manyReturns ->
    if options.decisionNodes then
      mkDecisionNodeSections from msg colors manyReturns
    else
      { nodes: [], edges: mkDirectEdges from msg colors manyReturns }

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
mkDirectEdges :: StateName -> MsgName -> ColorHarmony -> Array Return -> Array D.Edge
mkDirectEdges from msg colors returns = map
  ( case _ of
      Return to -> mkEdgeMsg from to colors msg
      ReturnVia guard to -> mkEdgeMsg from to colors (msg <> " ? " <> guard)
  )
  returns

-- | Creates a decision node structure for multiple returns from a single match.
mkDecisionNodeSections :: StateName -> MsgName -> ColorHarmony -> Array Return -> { nodes :: Array D.Node, edges :: Array D.Edge }
mkDecisionNodeSections from msg colors manyReturns =
  let
    decisionNode = mkDecisionNodeName from msg
  in
    { nodes: [ mkDecisionNode decisionNode colors ]
    , edges: [ mkEdgeMsg from decisionNode colors msg ]
        <> map (mkDecisionEdge decisionNode colors) manyReturns
    }

-- | Creates edges from a decision node to target states.
mkDecisionEdge :: StateName -> ColorHarmony -> Return -> D.Edge
mkDecisionEdge decisionNode colors = case _ of
  Return to -> mkEdgeGuard decisionNode to colors Nothing
  ReturnVia guard to -> mkEdgeGuard decisionNode to colors (Just guard)

-- | Creates global node attributes (default styling for all nodes).
mkGlobalNodeAttrs :: Options -> Array D.Attr
mkGlobalNodeAttrs options =
  join
    [ options.extraNodeAttrs
    , [ D.styleFilled
      , D.fontSize options.fontSize
      , D.fontNameArial
      , D.penWidth constants.nodePenWidth
      , D.labelLocC
      ]
    ]

-- | Creates global edge attributes (default styling for all edges).
mkGlobalEdgeAttrs :: Options -> Array D.Attr
mkGlobalEdgeAttrs options =
  join
    [ options.extraEdgeAttrs
    , [ D.arrowSize constants.arrowSize
      , D.penWidth constants.edgePenWidth
      , D.fontSize options.fontSize
      ]
    ]

-- | Creates global graph attributes.
mkGlobalGraphAttrs :: Options -> Array D.Attr
mkGlobalGraphAttrs options =
  join
    [ options.extraGraphAttrs
    , [ D.rankDirTD
      , D.labelLocT
      , D.fontSize options.fontSize
      , D.bgColor options.theme.bgColor
      , D.color options.theme.titleColor
      , D.fontColor options.theme.titleColor
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
mkStateNode options colors node = D.Node node
  $ join
      [ [ D.shapeBox
        , D.labelHtmlBold node
        ]
      , case options.layout of
          Manual positions ->
            case positions # Array.find (\position -> position.node == node) of
              Just { pos: { x: Inch x, y: Inch y }, exact: isExact } -> [ D.pos x y isExact ]
              Nothing -> []
          _ -> []
      , case options.fixedNodeSize of
          Just { x, y } ->
            [ D.width (unwrap x)
            , D.height (unwrap y)
            , D.fixedSize true
            ]
          Nothing ->
            [ D.height constants.nodeDefaultHeight
            ]
      , [ D.fontColor colors.nodeFont
        , D.fillColor colors.nodeBg
        , D.color colors.nodeBorder
        ]
      ]

-- | Creates an initialization node (entry point marker).
mkInitNode :: Options -> String -> D.Node
mkInitNode options name = D.Node (mkInitNodeName name)
  [ D.shapeCircle
  , D.label ""
  , D.width constants.initNodeSize
  , D.height constants.initNodeSize
  , D.fixedSize true
  , D.fillColor options.theme.initNodeColor
  ]

-- | Creates an edge from the initialization node to an entry point state.
mkInitEdge :: Options -> StateName -> StateName -> D.Edge
mkInitEdge options from to = D.Edge from to
  [ D.color options.theme.initNodeColor
  ]

-- | Creates an undirected edge (bidirectional) between two states.
mkUndirectedEdge :: Options -> StateName -> StateName -> MsgName -> D.Edge
mkUndirectedEdge options from to label = D.Edge from to
  [ D.labelHtmlBold label
  , D.dirBoth
  , D.color options.theme.undirectedEdgeColor
  , D.fontColor options.theme.undirectedEdgeFontColor
  ]

-- | Creates a directed edge with a message label.
mkEdgeMsg :: StateName -> StateName -> ColorHarmony -> MsgName -> D.Edge
mkEdgeMsg from to colors label = D.Edge from to
  [ D.labelHtmlBold label
  , D.color colors.edgeColor
  , D.fontColor colors.edgeFont
  ]

-- | Creates an edge from a decision node to a target state, optionally with a guard label.
mkEdgeGuard :: StateName -> StateName -> ColorHarmony -> Maybe GuardName -> D.Edge
mkEdgeGuard from to colors mayLabel = D.Edge from to
  $ catMaybes
      [ map D.labelHtmlItalic mayLabel
      , pure $ D.color colors.edgeColor
      , pure $ D.fontColor colors.edgeFont
      ]

-- | Creates a decision node (diamond shape) for branching transitions.
mkDecisionNode :: String -> ColorHarmony -> D.Node
mkDecisionNode name colors = D.Node name
  [ D.shapeDiamond
  , D.label "?"
  , D.fontColor colors.nodeFont
  , D.fillColor colors.nodeBg
  ]

-- | Validates that all specified entry points exist in the transit specification.
checkEntryPoints :: Array StateName -> TransitCore -> Either String Unit
checkEntryPoints entryPoints transitCore = do
  for_ entryPoints \entryPoint -> do
    if entryPoint `Array.elem` getStateNames transitCore then
      pure unit
    else
      Left $ "Entry point " <> entryPoint <> " not found in transit core"

-- | Validates that all manually positioned nodes exist in the transit specification.
checkPositions :: Array NodePositioning -> TransitCore -> Either String Unit
checkPositions positions transitCore = do
  for_ positions \position -> do
    if position.node `Array.elem` getStateNames transitCore then
      pure unit
    else
      Left $ "Node " <> position.node <> " not found in transit core"

-- | Validates all rendering options against the transit specification.
-- | Returns an error if entry points or manual node positions reference non-existent states.
checkOptions :: Options -> TransitCore -> Either String Unit
checkOptions options transitCore = do
  case options.layout of
    Manual positions -> checkPositions positions transitCore
    _ -> pure unit
  checkEntryPoints options.entryPoints transitCore

-- | Generates a Graphviz graph with customizable options. Fails if the options are invalid.
generateEither :: TransitCore -> (Options -> Options) -> Either String GraphvizGraph
generateEither transitCore mkOptions = do
  let
    options = mkOptions defaultOptions
  checkOptions options transitCore
  pure $ generate transitCore (\_ -> options)

-- | Generates a Graphviz graph with customizable options.
generate :: TransitCore -> (Options -> Options) -> GraphvizGraph
generate transitCore mkOptions =
  mkGraphvizGraph (mkOptions defaultOptions) transitCore
