module Transit.Gen.Graphviz where

import Prelude

import Color (Color)
import Data.Array as Array
import Data.Reflectable (class Reflectable, reflectType)
import Data.String as Str
import Effect (Effect)
import Node.Encoding (Encoding(..))
import Node.FS.Sync as FS
import Node.Path (FilePath)
import Transit.Core (Return, Return_(..), StateGraph_(..), Transition_(..))
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

mkGraphvizGraph :: Options -> StateGraph_ -> GraphvizGraph
mkGraphvizGraph options sg@(StateGraph transitions) = GraphvizGraph
  { global: []
  , nodes:
      [

      ] <> map mkNode (getStates sg)
  , edges: []
  }

mkNode :: String -> Node
mkNode stateName = Node stateName []

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
writeToFile mkOptions path = FS.writeTextFile
  UTF8
  path
  (toText (mkGraphvizGraph (mkOptions defaultOptions) $ reflectType (Proxy @spec)))

writeToFile_ :: forall @spec. Reflectable spec StateGraph_ => FilePath -> Effect Unit
writeToFile_ = writeToFile @spec identity

class ToText a where
  toText :: a -> String

newtype GraphvizGraph = GraphvizGraph
  { global :: Array Attr
  , nodes :: Array Node
  , edges :: Array Edge
  }

instance ToText GraphvizGraph where
  toText (GraphvizGraph { global, nodes, edges }) =
    "digraph " <> "" <> " {" <> "" <> Str.joinWith "\n" (map toText nodes) <> "}"

data Node = Node String (Array Attr)

instance ToText Node where
  toText (Node stateName attrs) = stateName <> " [" <> "" <> "]"

data Edge = Edge String String (Array Attr)

instance ToText Edge where
  toText (Edge from to attrs) = from <> " -> " <> to <> " [" <> "" <> "]"

data Attr = Attr String Value

data Value = Value String | ValueColor Color