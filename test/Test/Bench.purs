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
import Test.BenchDef.Transit (Msg(..), State(..), update, updateClassic)
import Test.BenchDef.TransitVariant (StateV, MsgV, updateV)
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
inputs = [ State01 /\ Msg01, State02 /\ Msg02, State03 /\ Msg03, State04 /\ Msg04, State05 /\ Msg05, State06 /\ Msg06, State07 /\ Msg07, State08 /\ Msg08, State09 /\ Msg09, State10 /\ Msg10, State11 /\ Msg11, State12 /\ Msg12, State13 /\ Msg13, State14 /\ Msg14, State15 /\ Msg15, State16 /\ Msg16, State17 /\ Msg17, State18 /\ Msg18, State19 /\ Msg19, State20 /\ Msg20, State21 /\ Msg21, State22 /\ Msg22, State23 /\ Msg23, State24 /\ Msg24, State25 /\ Msg25, State26 /\ Msg26, State27 /\ Msg27, State28 /\ Msg28, State29 /\ Msg29, State30 /\ Msg30, State31 /\ Msg31, State32 /\ Msg32, State33 /\ Msg33, State34 /\ Msg34, State35 /\ Msg35, State36 /\ Msg36, State37 /\ Msg37, State38 /\ Msg38, State39 /\ Msg39, State40 /\ Msg40, State41 /\ Msg41, State42 /\ Msg42, State43 /\ Msg43, State44 /\ Msg44, State45 /\ Msg45, State46 /\ Msg46, State47 /\ Msg47, State48 /\ Msg48, State49 /\ Msg49, State50 /\ Msg50 ]

inputsV :: Array (StateV /\ MsgV)
inputsV = [ V.inj (Proxy @"State01") {} /\ V.inj (Proxy @"Msg01") {}, V.inj (Proxy @"State02") {} /\ V.inj (Proxy @"Msg02") {}, V.inj (Proxy @"State03") {} /\ V.inj (Proxy @"Msg03") {}, V.inj (Proxy @"State04") {} /\ V.inj (Proxy @"Msg04") {}, V.inj (Proxy @"State05") {} /\ V.inj (Proxy @"Msg05") {}, V.inj (Proxy @"State06") {} /\ V.inj (Proxy @"Msg06") {}, V.inj (Proxy @"State07") {} /\ V.inj (Proxy @"Msg07") {}, V.inj (Proxy @"State08") {} /\ V.inj (Proxy @"Msg08") {}, V.inj (Proxy @"State09") {} /\ V.inj (Proxy @"Msg09") {}, V.inj (Proxy @"State10") {} /\ V.inj (Proxy @"Msg10") {}, V.inj (Proxy @"State11") {} /\ V.inj (Proxy @"Msg11") {}, V.inj (Proxy @"State12") {} /\ V.inj (Proxy @"Msg12") {}, V.inj (Proxy @"State13") {} /\ V.inj (Proxy @"Msg13") {}, V.inj (Proxy @"State14") {} /\ V.inj (Proxy @"Msg14") {}, V.inj (Proxy @"State15") {} /\ V.inj (Proxy @"Msg15") {}, V.inj (Proxy @"State16") {} /\ V.inj (Proxy @"Msg16") {}, V.inj (Proxy @"State17") {} /\ V.inj (Proxy @"Msg17") {}, V.inj (Proxy @"State18") {} /\ V.inj (Proxy @"Msg18") {}, V.inj (Proxy @"State19") {} /\ V.inj (Proxy @"Msg19") {}, V.inj (Proxy @"State20") {} /\ V.inj (Proxy @"Msg20") {}, V.inj (Proxy @"State21") {} /\ V.inj (Proxy @"Msg21") {}, V.inj (Proxy @"State22") {} /\ V.inj (Proxy @"Msg22") {}, V.inj (Proxy @"State23") {} /\ V.inj (Proxy @"Msg23") {}, V.inj (Proxy @"State24") {} /\ V.inj (Proxy @"Msg24") {}, V.inj (Proxy @"State25") {} /\ V.inj (Proxy @"Msg25") {}, V.inj (Proxy @"State26") {} /\ V.inj (Proxy @"Msg26") {}, V.inj (Proxy @"State27") {} /\ V.inj (Proxy @"Msg27") {}, V.inj (Proxy @"State28") {} /\ V.inj (Proxy @"Msg28") {}, V.inj (Proxy @"State29") {} /\ V.inj (Proxy @"Msg29") {}, V.inj (Proxy @"State30") {} /\ V.inj (Proxy @"Msg30") {}, V.inj (Proxy @"State31") {} /\ V.inj (Proxy @"Msg31") {}, V.inj (Proxy @"State32") {} /\ V.inj (Proxy @"Msg32") {}, V.inj (Proxy @"State33") {} /\ V.inj (Proxy @"Msg33") {}, V.inj (Proxy @"State34") {} /\ V.inj (Proxy @"Msg34") {}, V.inj (Proxy @"State35") {} /\ V.inj (Proxy @"Msg35") {}, V.inj (Proxy @"State36") {} /\ V.inj (Proxy @"Msg36") {}, V.inj (Proxy @"State37") {} /\ V.inj (Proxy @"Msg37") {}, V.inj (Proxy @"State38") {} /\ V.inj (Proxy @"Msg38") {}, V.inj (Proxy @"State39") {} /\ V.inj (Proxy @"Msg39") {}, V.inj (Proxy @"State40") {} /\ V.inj (Proxy @"Msg40") {}, V.inj (Proxy @"State41") {} /\ V.inj (Proxy @"Msg41") {}, V.inj (Proxy @"State42") {} /\ V.inj (Proxy @"Msg42") {}, V.inj (Proxy @"State43") {} /\ V.inj (Proxy @"Msg43") {}, V.inj (Proxy @"State44") {} /\ V.inj (Proxy @"Msg44") {}, V.inj (Proxy @"State45") {} /\ V.inj (Proxy @"Msg45") {}, V.inj (Proxy @"State46") {} /\ V.inj (Proxy @"Msg46") {}, V.inj (Proxy @"State47") {} /\ V.inj (Proxy @"Msg47") {}, V.inj (Proxy @"State48") {} /\ V.inj (Proxy @"Msg48") {}, V.inj (Proxy @"State49") {} /\ V.inj (Proxy @"Msg49") {}, V.inj (Proxy @"State50") {} /\ V.inj (Proxy @"Msg50") {} ]

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