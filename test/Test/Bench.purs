module Test.Bench (main) where

import Prelude

import BenchLib (bench, group, reportConsole, suite_)
import BenchLib as BenchLib
import BenchLib.Reporters.Json (reportJson_)
import BenchLib.Reporters.VegaLite (reportVegaLite)
import Data.Array as Array
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Node.Process (lookupEnv)
import Partial.Unsafe (unsafeCrashWith)
import Test.BenchDef.Transit (StateD(..), getInputs, getInputsD, printMsg, printMsgD, printState, printStateD, update, updateClassic)
import Transit.VariantUtils (v)

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
        , reportJson_
        ]
    } $
    suite_
      ("Benchmarks for " <> backend <> " backend")
      [ group "Update Functions"
          _
            { iterations = iterations
            , sizes = [ 50, 100, 200, 400, 800, 1600, 3200 ]
            }
          [ bench
              "updateClassic"
              ( _
                  { normIn = map printMsgD
                  , normOut = map printStateD
                  }
              )
              { prepare: getInputsD
              , run: \msgs ->
                  Array.scanl updateClassic (State01 {}) msgs
              }
          , bench
              "update"
              ( _
                  { normIn = map printMsg
                  , normOut = map printState
                  }
              )
              { prepare: \size -> getInputs size
              , run: \msgs -> Array.scanl update (v @"State01") msgs
              }
          ]
      ]

showPad :: Int -> String
showPad n = if n < 10 then "0" <> show n else show n