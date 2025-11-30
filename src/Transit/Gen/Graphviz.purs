module Transit.Gen.Graphviz where

import Prelude

import Color (rgb)
import Data.DotLang (node, (=*>), (==>))
import Data.DotLang as Dot
import Data.DotLang.Attr (FillStyle(..))
import Data.DotLang.Attr.Edge as Edge
import Data.DotLang.Attr.Node as Node
import Data.DotLang.Class (toText)
import Effect (Effect)
import Node.Encoding (Encoding(..))
import Node.FS.Sync as FS
import Node.Path (FilePath)
import Transit.Core (StateGraph_(..))
import Unsafe.Coerce (unsafeCoerce)

type GraphizGraph = {}

exampleGraph = Dot.DiGraph
  [ node "a" [ Node.Shape Node.Diamond, Node.Style Node.Filled, Node.FillColor (rgb 255 0 0) ]
  , node "b" []
  , "a" ==> "b"
  , "a" =*> "d" $ [ Edge.FillColor (rgb 255 0 0) ]
  ]

f :: StateGraph_ -> GraphizGraph
f = unsafeCoerce "todo"

g :: GraphizGraph -> String
g = unsafeCoerce "todo"

type Options =
  { title :: String
  }

defaultOptions :: Options
defaultOptions =
  { title: "Untitled"
  }

h :: forall @spec. (Options -> Options) -> FilePath -> Effect Unit
h = unsafeCoerce "todo"

writeToFile :: forall @spec. FilePath -> Effect Unit
writeToFile path = FS.writeTextFile UTF8 path (toText exampleGraph)