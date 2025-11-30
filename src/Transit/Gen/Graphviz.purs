module Transit.Gen.Graphviz where

import Prelude

import Effect (Effect)
import Node.Encoding (Encoding(..))
import Node.FS.Sync as FS
import Node.Path (FilePath)
import Transit.Core (StateGraph_(..))
import Unsafe.Coerce (unsafeCoerce)

type GraphizGraph = {}

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
writeToFile path = FS.writeTextFile UTF8 path "digraph Sample { a -> b; }"