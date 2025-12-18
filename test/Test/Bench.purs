module Test.Bench (main) where

import Prelude

import BenchLib (bench_, group, reportConsole, suite_)
import BenchLib as BenchLib
import BenchLib.Reporters.VegaLite (reportVegaLite)
import Data.Array ((!!))
import Data.Array as Array
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Node.Process (lookupEnv)
import Partial.Unsafe (unsafeCrashWith)
import Test.BenchDef.Transit (inputs, inputsD, update, updateClassic)

type Config =
  { backend :: String
  , iterations :: Int
  }

getConfigFromEnv :: Effect Config
getConfigFromEnv = do
  backend <- lookupEnv "BACKEND" >>= case _ of
    Just backend -> pure backend
    _ -> unsafeCrashWith "BACKEND environment variable must be set to JS or ES"

  iterations <- lookupEnv "ITERATIONS" >>= case _ of
    Just iterations | Just i <- Int.fromString iterations -> pure i
    _ -> unsafeCrashWith "ITERATIONS environment variable must be set to an integer"

  pure { backend, iterations }

main :: Effect Unit
main = do
  { backend, iterations } <- getConfigFromEnv

  BenchLib.runNode _
    { reporters =
        [ reportConsole
        , reportVegaLite _ { folderPath = "bench/backend-" <> backend }
        ]
    } $
    suite_
      ("Benchmarks for " <> backend <> " backend")
      [ group "Update Functions"
          _
            { iterations = iterations
            , sizes = Array.range 1 (Array.length inputs - 1)
            }
          [ bench_
              "updateClassic"
              { prepare: \size -> case inputsD !! size of
                  Just input -> input
                  _ -> unsafeCrashWith "Invalid size"
              , run: \(state /\ msg) -> updateClassic state msg
              }

          , bench_
              "update"
              { prepare: \size -> case inputs !! size of
                  Just input -> input
                  _ -> unsafeCrashWith "Invalid size"
              , run: \(state /\ msg) -> update state msg
              }
          ]
      ]