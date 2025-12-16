module Test.Bench (main) where

import Prelude

import BenchLib (bench_, group, reportConsole, suite_)
import BenchLib as BenchLib
import BenchLib.Reporters.Markdown (reportMarkdown)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple.Nested ((/\))
import Data.Variant as V
import Effect (Effect)
import Node.Process (lookupEnv)
import Partial.Unsafe (unsafeCrashWith)
import Test.BenchDefs (State(..), Msg(..), update, updateClassic, updateV)
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

main :: Effect Unit
main = do
  { backend, iterations } <- getConfigFromEnv

  BenchLib.runNode _
    { reporters =
        [ reportMarkdown _
            { filePath = "bench/backend-" <> backend <> ".md"
            , maxTime = Milliseconds 0.025
            }
        , reportConsole
        ]
    } $
    suite_
      ("Benchmarks for " <> backend <> " backend")
      [ group "Update Functions"
          _
            { iterations = iterations
            , sizes = [ 5, 10, 15, 20 ]
            }
          [ bench_
              "update (Transit with ADTs)"
              { prepare: \size -> case size of
                  5 -> State05 /\ Msg05
                  10 -> State10 /\ Msg10
                  15 -> State15 /\ Msg15
                  20 -> State20 /\ Msg20
                  _ -> unsafeCrashWith "Invalid size"
              , run: \(state /\ msg) -> update state msg
              }

          , bench_
              "updateClassic"
              { prepare: \size -> case size of
                  5 -> State05 /\ Msg05
                  10 -> State10 /\ Msg10
                  15 -> State15 /\ Msg15
                  20 -> State20 /\ Msg20
                  _ -> unsafeCrashWith "Invalid size"
              , run: \(state /\ msg) -> updateClassic state msg
              }

          , bench_
              "updateV (Transit with Variants)"
              { prepare: \size -> case size of
                  5 -> V.inj (Proxy @"State05") unit /\ V.inj (Proxy @"Msg05") unit
                  10 -> V.inj (Proxy @"State10") unit /\ V.inj (Proxy @"Msg10") unit
                  15 -> V.inj (Proxy @"State15") unit /\ V.inj (Proxy @"Msg15") unit
                  20 -> V.inj (Proxy @"State20") unit /\ V.inj (Proxy @"Msg20") unit
                  _ -> unsafeCrashWith "Invalid size"
              , run: \(state /\ msg) -> updateV state msg
              }
          ]
      ]