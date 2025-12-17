module Test.Bench (main) where

import Prelude

import BenchLib (bench_, group, reportConsole, suite_)
import BenchLib as BenchLib
import BenchLib.Reporters.Markdown (reportMarkdown)
import BenchLib.Reporters.VegaLite (reportVegaLite, reportVegaLite_)
import Data.Array (range, (!!))
import Data.Array as Array
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Number as Number
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple.Nested (type (/\), (/\))
import Data.Variant as V
import Effect (Effect)
import Node.Process (lookupEnv)
import Partial.Unsafe (unsafeCrashWith)
import Test.BenchDef.Transit (Msg(..), MsgD(..), State(..), StateD(..), inputs, inputsD, update, updateClassic)
import Transit.VariantUtils (v)
import Type.Proxy (Proxy(..))

type Config =
  { backend :: String
  , iterations :: Int
  , maxTime :: Milliseconds
  }

getConfigFromEnv :: Effect Config
getConfigFromEnv = do
  backend <- lookupEnv "BACKEND" >>= case _ of
    Just backend -> pure backend
    _ -> unsafeCrashWith "BACKEND environment variable must be set to JS or ES"

  iterations <- lookupEnv "ITERATIONS" >>= case _ of
    Just iterations | Just i <- Int.fromString iterations -> pure i
    _ -> unsafeCrashWith "ITERATIONS environment variable must be set to an integer"

  maxTime <- lookupEnv "MAX_TIME" >>= case _ of
    Just maxTime | Just ms <- Number.fromString maxTime -> pure (Milliseconds ms)
    _ -> unsafeCrashWith "MAX_TIME environment variable must be set to a duration in milliseconds"

  pure { backend, iterations, maxTime }

main :: Effect Unit
main = do
  { backend, iterations, maxTime } <- getConfigFromEnv

  BenchLib.runNode _
    { reporters =
        [ reportMarkdown _
            { filePath = "bench/backend-" <> backend <> ".md"
            , maxTime = maxTime
            }
        , reportConsole
        , reportVegaLite_
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