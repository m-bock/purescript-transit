module Transit.StateGraph where

import Prelude

import Data.Maybe (Maybe)
import Transit.Graph (Graph)

type Edge = { msg :: String, guard :: Maybe String }

type Node = { state :: String }

type StateGraph = Graph Edge Node