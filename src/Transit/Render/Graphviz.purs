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

import Data.Array (catMaybes, concatMap, mapWithIndex)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, unwrap)
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
    , case options.rawGlobalAttrs of
        Just raw -> [ SecGlobalRaw raw ]
        Nothing -> []
    , join $ mapWithIndex (mkStateSections transit options) $ getStateNames transit
    ]

-- | Creates sections for a single state (node, entry point edges, and transition edges).
mkStateSections :: TransitCore -> Options -> Int -> StateName -> Array D.Section
mkStateSections transit options i stateName = join
  [ pure $ SecNode $ mkStateNode options colors stateName
  , if Array.elem stateName options.entryPoints then
      [ SecNode $ mkInitNode options constants.initNodeName
      , SecEdge $ mkInitEdge options constants.initNodeName stateName
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
    if options.undirectedEdges && hasComplementaryEdge from to msg transit then
      if isCanonicalFirst from to then
        [ SecEdge $ mkUndirectedEdge options from to msg ]
      else
        []
    else
      [ SecEdge $ mkEdgeMsg options from to colors msg ]
  manyReturns ->
    if options.decisionNodes then
      mkDecisionNodeSections options from msg colors manyReturns
    else
      mkDirectEdges options from msg colors manyReturns

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
mkDirectEdges :: Options -> StateName -> MsgName -> ColorHarmony -> Array Return -> Array D.Section
mkDirectEdges options from msg colors returns = Array.concatMap
  ( case _ of
      Return to -> [ SecEdge $ mkEdgeMsg options from to colors msg ]
      ReturnVia guard to -> [ SecEdge $ mkEdgeMsg options from to colors (msg <> " ? " <> guard) ]
  )
  returns

-- | Creates a decision node structure for multiple returns from a single match.
mkDecisionNodeSections :: Options -> StateName -> MsgName -> ColorHarmony -> Array Return -> Array D.Section
mkDecisionNodeSections options from msg colors manyReturns =
  let
    decisionNode = constants.decisionNodePrefix <> from <> "_" <> msg
  in
    join
      [ pure $ SecNode $ mkDecisionNode options decisionNode colors
      , pure $ SecEdge $ mkEdgeMsg options from decisionNode colors msg
      , concatMap (mkDecisionEdges options decisionNode colors) manyReturns
      ]

-- | Creates edges from a decision node to target states.
mkDecisionEdges :: Options -> StateName -> ColorHarmony -> Return -> Array D.Section
mkDecisionEdges options decisionNode colors = case _ of
  Return to -> [ SecEdge $ mkEdgeGuard options decisionNode to colors Nothing ]
  ReturnVia guard to -> [ SecEdge $ mkEdgeGuard options decisionNode to colors (Just guard) ]

-- | Creates global graph attributes.
mkGlobalAttrs :: Options -> Array D.Attr
mkGlobalAttrs options =
  join
    [ [ D.rankDirTD
      , D.fontNameArial
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
mkStateNode options colors node = D.Node node (options.rawNodeAttrs # map (\f -> f node))
  $ join
      [ [ D.shapeBox
        , D.labelHtmlBold node
        , D.fontSize options.fontSize
        , D.styleFilled
        , D.fillColor colors.nodeBg
        , D.fontColor colors.nodeFont
        , D.color colors.nodeBorder
        , D.fontNameArial
        , D.labelLocC
        , D.penWidth constants.nodePenWidth
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
      ]

-- | Creates an initialization node (entry point marker).
mkInitNode :: Options -> String -> D.Node
mkInitNode options name = D.Node name Nothing
  [ D.shapeCircle
  , D.label ""
  , D.width constants.initNodeSize
  , D.height constants.initNodeSize
  , D.fixedSize true
  , D.styleFilled
  , D.fillColor options.theme.initNodeColor
  , D.penWidth constants.nodePenWidth
  ]

-- | Creates an edge from the initialization node to an entry point state.
mkInitEdge :: Options -> StateName -> StateName -> D.Edge
mkInitEdge options from to = D.Edge from to
  [ D.color options.theme.initNodeColor
  , D.fontSize options.fontSize
  , D.arrowSize constants.arrowSize
  , D.penWidth constants.edgePenWidth
  ]

-- | Creates an undirected edge (bidirectional) between two states.
mkUndirectedEdge :: Options -> StateName -> StateName -> MsgName -> D.Edge
mkUndirectedEdge options from to label = D.Edge from to
  [ D.color options.theme.undirectedEdgeColor
  , D.fontColor options.theme.undirectedEdgeFontColor
  , D.fontSize options.fontSize
  , D.labelHtmlBold label
  , D.arrowSize constants.arrowSize
  , D.penWidth constants.edgePenWidth
  , D.dirBoth
  ]

-- | Creates a directed edge with a message label.
mkEdgeMsg :: Options -> StateName -> StateName -> ColorHarmony -> MsgName -> D.Edge
mkEdgeMsg options from to colors label = D.Edge from to
  [ D.color colors.edgeColor
  , D.fontColor colors.edgeFont
  , D.fontSize options.fontSize
  , D.arrowSize constants.arrowSize
  , D.labelHtmlBold label
  , D.penWidth constants.edgePenWidth
  ]

-- | Creates an edge from a decision node to a target state, optionally with a guard label.
mkEdgeGuard :: Options -> StateName -> StateName -> ColorHarmony -> Maybe GuardName -> D.Edge
mkEdgeGuard options from to colors mayLabel = D.Edge from to
  $ catMaybes
      [ pure $ D.color colors.edgeColor
      , pure $ D.fontColor colors.edgeFont
      , pure $ D.fontSize options.fontSize
      , pure $ D.arrowSize constants.arrowSize
      , map D.labelHtmlItalic mayLabel
      , pure $ D.penWidth constants.edgePenWidth
      ]

-- | Creates a decision node (diamond shape) for branching transitions.
mkDecisionNode :: Options -> String -> ColorHarmony -> D.Node
mkDecisionNode options name colors = D.Node name Nothing
  [ D.shapeDiamond
  , D.label "?"
  , D.fontSize options.fontSize
  , D.fontColor colors.nodeFont
  , D.styleFilled
  , D.fillColor colors.nodeBg
  , D.penWidth constants.nodePenWidth
  ]

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
  , rawGlobalAttrs :: Maybe String
  , rawNodeAttrs :: Maybe (StateName -> String)
  }

type Constants =
  { arrowSize :: Number
  , edgePenWidth :: Number
  , nodePenWidth :: Number
  , nodeDefaultHeight :: Number
  , initNodeSize :: Number
  , initNodeName :: String
  , decisionNodePrefix :: String
  }

constants :: Constants
constants =
  { arrowSize: 0.7
  , edgePenWidth: 1.8
  , nodePenWidth: 0.0
  , nodeDefaultHeight: 0.4
  , initNodeSize: 0.15
  , initNodeName: "__Start__"
  , decisionNodePrefix: "decision_"
  }

type Vec = { x :: Inch, y :: Inch }

type NodePositioning =
  { node :: String
  , pos :: Vec
  , exact :: Boolean
  }

data Layout
  = Landscape
  | Portrait
  | Manual (Array NodePositioning)
  | Circle
  | None

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
  , rawGlobalAttrs: Nothing
  , rawNodeAttrs: Nothing
  }

newtype Inch = Inch Number

derive instance Newtype Inch _

checkEntryPoints :: Array StateName -> TransitCore -> Either String Unit
checkEntryPoints entryPoints transitCore = do
  for_ entryPoints \entryPoint -> do
    if entryPoint `Array.elem` getStateNames transitCore then
      pure unit
    else
      Left $ "Entry point " <> entryPoint <> " not found in transit core"

checkPositions :: Array NodePositioning -> TransitCore -> Either String Unit
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
  let
    options = mkOptions defaultOptions
  checkOptions options transitCore
  pure $ generate transitCore (\_ -> options)

-- | Generates a Graphviz graph with customizable options.
generate :: TransitCore -> (Options -> Options) -> GraphvizGraph
generate transitCore mkOptions =
  mkGraphvizGraph (mkOptions defaultOptions) transitCore
