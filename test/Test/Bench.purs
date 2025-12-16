module Test.Bench (main) where

import Prelude

import BenchLib (bench_, group, reportConsole, suite_)
import BenchLib as BenchLib
import BenchLib.Reporters.Markdown (reportMarkdown)
import Data.Array (range, (!!))
import Data.Array as Array
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple.Nested (type (/\), (/\))
import Data.Variant as V
import Effect (Effect)
import Node.Process (lookupEnv)
import Partial.Unsafe (unsafeCrashWith)
import Test.BenchDefs (Msg(..), State(..), StateV, MsgV, update, updateClassic, updateV)
import Type.Proxy (Proxy(..))

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

inputs :: Array (State /\ Msg)
inputs = [ State01 /\ Msg01, State02 /\ Msg02, State03 /\ Msg03, State04 /\ Msg04, State05 /\ Msg05, State06 /\ Msg06, State07 /\ Msg07, State08 /\ Msg08, State09 /\ Msg09, State10 /\ Msg10, State11 /\ Msg11, State12 /\ Msg12, State13 /\ Msg13, State14 /\ Msg14, State15 /\ Msg15, State16 /\ Msg16, State17 /\ Msg17, State18 /\ Msg18, State19 /\ Msg19, State20 /\ Msg20 ]

inputsV :: Array (StateV /\ MsgV)
inputsV = [ V.inj (Proxy @"State01") unit /\ V.inj (Proxy @"Msg01") unit, V.inj (Proxy @"State02") unit /\ V.inj (Proxy @"Msg02") unit, V.inj (Proxy @"State03") unit /\ V.inj (Proxy @"Msg03") unit, V.inj (Proxy @"State04") unit /\ V.inj (Proxy @"Msg04") unit, V.inj (Proxy @"State05") unit /\ V.inj (Proxy @"Msg05") unit, V.inj (Proxy @"State06") unit /\ V.inj (Proxy @"Msg06") unit, V.inj (Proxy @"State07") unit /\ V.inj (Proxy @"Msg07") unit, V.inj (Proxy @"State08") unit /\ V.inj (Proxy @"Msg08") unit, V.inj (Proxy @"State09") unit /\ V.inj (Proxy @"Msg09") unit, V.inj (Proxy @"State10") unit /\ V.inj (Proxy @"Msg10") unit, V.inj (Proxy @"State11") unit /\ V.inj (Proxy @"Msg11") unit, V.inj (Proxy @"State12") unit /\ V.inj (Proxy @"Msg12") unit, V.inj (Proxy @"State13") unit /\ V.inj (Proxy @"Msg13") unit, V.inj (Proxy @"State14") unit /\ V.inj (Proxy @"Msg14") unit, V.inj (Proxy @"State15") unit /\ V.inj (Proxy @"Msg15") unit, V.inj (Proxy @"State16") unit /\ V.inj (Proxy @"Msg16") unit, V.inj (Proxy @"State17") unit /\ V.inj (Proxy @"Msg17") unit, V.inj (Proxy @"State18") unit /\ V.inj (Proxy @"Msg18") unit, V.inj (Proxy @"State19") unit /\ V.inj (Proxy @"Msg19") unit, V.inj (Proxy @"State20") unit /\ V.inj (Proxy @"Msg20") unit ]

main :: Effect Unit
main = do
  { backend, iterations } <- getConfigFromEnv

  BenchLib.runNode _
    { reporters =
        [ reportMarkdown _
            { filePath = "bench/backend-" <> backend <> ".md"
            , maxTime = Milliseconds case backend of
                "JS" -> 0.05
                "ES" -> 0.05
                _ -> unsafeCrashWith "Invalid backend"
            }
        , reportConsole
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
              "update (Transit with ADTs)"
              { prepare: \size -> case inputs !! size of
                  Just input -> input
                  _ -> unsafeCrashWith "Invalid size"
              , run: \(state /\ msg) -> update state msg
              }

          , bench_
              "updateClassic"
              { prepare: \size -> case inputs !! size of
                  Just input -> input
                  _ -> unsafeCrashWith "Invalid size"
              , run: \(state /\ msg) -> updateClassic state msg
              }

          , bench_
              "updateV (Transit with Variants)"
              { prepare: \size -> case inputsV !! size of
                  Just input -> input
                  _ -> unsafeCrashWith "Invalid size"
              , run: \(state /\ msg) -> updateV state msg
              }
          ]
      ]