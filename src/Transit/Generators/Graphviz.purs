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
import Transit.Colors
import Transit.Colors as Colors
import Transit.Core (Match(..), Return(..), TransitCore(..), getMatchesForState, getStateNames)
import Transit.Data.DotLang (GlobalAttrs(..), GraphvizGraph(..), Section(..), toText)
import Transit.Data.DotLang as D
import Transit.StateGraph (StateNode)

mkGraphvizGraph :: Options -> TransitCore -> GraphvizGraph
mkGraphvizGraph options transit =
  GraphvizGraph $ join
    [ pure $ SecGlobal $ GlobalAttrs $ mkGlobalAttrs options
    , case options.globalAttrsRaw of
        Just raw -> [ SecGlobalRaw raw ]
        Nothing -> []
    , join $ mapWithIndex (mkStateSections transit options) $ getStateNames transit
    ]

mkStateSections :: TransitCore -> Options -> Int -> String -> Array D.Section
mkStateSections transit options i stateName = join
  [ pure $ SecNode $ mkStateNode options colors stateName
  , if i == 0 then
      [ SecNode $ mkInitNode "__Start__"
      , SecEdge $ mkInitEdge "__Start__" stateName
      ]
    else []
  , Array.concatMap (mkMatchSections colors transit options) $ getMatchesForState stateName transit
  ]
  where
  colors = getColorHarmony options.theme i

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

isCanonicalFirst :: String -> String -> Boolean
isCanonicalFirst from to = (from > to)

hasComplementaryEdge :: String -> String -> String -> TransitCore -> Boolean
hasComplementaryEdge from to msg (TransitCore matches) =
  Array.any (\(Match from' msg' returns') -> from' == to && msg' == msg && returns' == [ Return from ]) matches

mkDirectEdges :: String -> String -> ColorHarmony -> Array Return -> Array D.Section
mkDirectEdges from msg colors returns = Array.concatMap
  ( case _ of
      Return to -> [ SecEdge $ mkEdgeMsg from to colors msg ]
      ReturnVia guard to -> [ SecEdge $ mkEdgeMsg from to colors (msg <> " ? " <> guard) ]
  )
  returns

mkDecisionNodeSections :: String -> String -> ColorHarmony -> Array Return -> Array D.Section
mkDecisionNodeSections from msg colors manyReturns =
  let
    decisionNode = "decision_" <> from <> "_" <> msg
  in
    join
      [ pure $ SecNode $ mkDecisionNode decisionNode colors
      , pure $ SecEdge $ mkEdgeMsg from decisionNode colors msg
      , concatMap (mkDecisionEdges decisionNode colors) manyReturns
      ]

mkDecisionEdges :: String -> ColorHarmony -> Return -> Array D.Section
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
  , D.bgColor options.theme.bgColor
  ]

mkStateNode :: Options -> ColorHarmony -> StateNode -> D.Node
mkStateNode options colors node = D.Node node (options.nodeAttrsRaw # map (\f -> f node))
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
  , D.arrowSize 0.7
  ]

mkEdgeMsg :: String -> String -> ColorHarmony -> String -> D.Edge
mkEdgeMsg from to colors label = D.Edge from to
  [ D.color colors.edgeColor
  , D.fontColor colors.edgeFont
  , D.fontSize 12
  , D.arrowSize 0.7
  , D.labelHtmlBold label
  , D.penWidth 1.8
  ]

mkEdgeGuard :: String -> String -> ColorHarmony -> Maybe String -> D.Edge
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

type Options =
  { title :: String
  , theme :: Theme
  , globalAttrsRaw :: Maybe String
  , nodeAttrsRaw :: Maybe (String -> String)
  , useDecisionNodes :: Boolean
  , useUndirectedEdges :: Boolean
  }

defaultOptions :: Options
defaultOptions =
  { title: "Untitled"
  , theme: themeHarmonyLight
  , globalAttrsRaw: Nothing
  , nodeAttrsRaw: Nothing
  , useDecisionNodes: true
  , useUndirectedEdges: false
  }

writeToFile :: FilePath -> TransitCore -> (Options -> Options) -> Effect Unit
writeToFile path sg mkOptions = do
  FS.writeTextFile UTF8 path
    (toText (mkGraphvizGraph (mkOptions defaultOptions) sg))
  Console.log $ "Wrote graphviz graph to " <> path

writeToFile_ :: FilePath -> TransitCore -> Effect Unit
writeToFile_ path sg = writeToFile path sg identity

