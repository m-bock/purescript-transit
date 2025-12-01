module Transit.Gen.Graphviz where

import Prelude

import Data.Array as Array
import Data.Reflectable (class Reflectable, reflectType)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Class.Console as Console
import Node.Encoding (Encoding(..))
import Node.FS.Sync as FS
import Node.Path (FilePath)
import Transit.Core (Return_(..), StateGraph_(..), Transition_(..))
import Transit.DotLang (Edge(..), GraphvizGraph(..), Node(..), toText)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

mkGraphvizGraph :: Options -> StateGraph_ -> GraphvizGraph
mkGraphvizGraph options sg@(StateGraph transitions) = GraphvizGraph
  { global: []
  , nodes:
      [

      ] <> map mkNode (getStates sg)
  , edges: map mkEdge (getEdges sg)
  }

mkNode :: String -> Node
mkNode stateName = Node stateName []

mkEdge :: String /\ String -> Edge
mkEdge (from /\ to) = Edge from to []

getEdges :: StateGraph_ -> Array (String /\ String)
getEdges (StateGraph transitions) =
  map getEdge transitions
  where
  getEdge = case _ of
    (Transition from _ [ Return to ]) -> from /\ to
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
  }

defaultOptions :: Options
defaultOptions =
  { title: "Untitled"
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

