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
import Test.BenchDef.Transit (Msg(..), MsgD(..), State(..), StateD(..), update, updateClassic)
import Transit.VariantUtils (inj)
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
inputs =
  [ inj @"State01" /\ inj @"Msg01"
  , inj @"State02" /\ inj @"Msg02"
  , inj @"State03" /\ inj @"Msg03"
  , inj @"State04" /\ inj @"Msg04"
  , inj @"State05" /\ inj @"Msg05"
  , inj @"State06" /\ inj @"Msg06"
  , inj @"State07" /\ inj @"Msg07"
  , inj @"State08" /\ inj @"Msg08"
  , inj @"State09" /\ inj @"Msg09"
  , inj @"State10" /\ inj @"Msg10"
  , inj @"State11" /\ inj @"Msg11"
  , inj @"State12" /\ inj @"Msg12"
  , inj @"State13" /\ inj @"Msg13"
  , inj @"State14" /\ inj @"Msg14"
  , inj @"State15" /\ inj @"Msg15"
  , inj @"State16" /\ inj @"Msg16"
  , inj @"State17" /\ inj @"Msg17"
  , inj @"State18" /\ inj @"Msg18"
  , inj @"State19" /\ inj @"Msg19"
  , inj @"State20" /\ inj @"Msg20"
  , inj @"State21" /\ inj @"Msg21"
  , inj @"State22" /\ inj @"Msg22"
  , inj @"State23" /\ inj @"Msg23"
  , inj @"State24" /\ inj @"Msg24"
  , inj @"State25" /\ inj @"Msg25"
  , inj @"State26" /\ inj @"Msg26"
  , inj @"State27" /\ inj @"Msg27"
  , inj @"State28" /\ inj @"Msg28"
  , inj @"State29" /\ inj @"Msg29"
  , inj @"State30" /\ inj @"Msg30"
  , inj @"State31" /\ inj @"Msg31"
  , inj @"State32" /\ inj @"Msg32"
  , inj @"State33" /\ inj @"Msg33"
  , inj @"State34" /\ inj @"Msg34"
  , inj @"State35" /\ inj @"Msg35"
  , inj @"State36" /\ inj @"Msg36"
  , inj @"State37" /\ inj @"Msg37"
  , inj @"State38" /\ inj @"Msg38"
  , inj @"State39" /\ inj @"Msg39"
  , inj @"State40" /\ inj @"Msg40"
  , inj @"State41" /\ inj @"Msg41"
  , inj @"State42" /\ inj @"Msg42"
  , inj @"State43" /\ inj @"Msg43"
  , inj @"State44" /\ inj @"Msg44"
  , inj @"State45" /\ inj @"Msg45"
  , inj @"State46" /\ inj @"Msg46"
  , inj @"State47" /\ inj @"Msg47"
  , inj @"State48" /\ inj @"Msg48"
  , inj @"State49" /\ inj @"Msg49"
  , inj @"State50" /\ inj @"Msg50"
  ]

inputsD :: Array (StateD /\ MsgD)
inputsD = [ State01 /\ Msg01, State02 /\ Msg02, State03 /\ Msg03, State04 /\ Msg04, State05 /\ Msg05, State06 /\ Msg06, State07 /\ Msg07, State08 /\ Msg08, State09 /\ Msg09, State10 /\ Msg10, State11 /\ Msg11, State12 /\ Msg12, State13 /\ Msg13, State14 /\ Msg14, State15 /\ Msg15, State16 /\ Msg16, State17 /\ Msg17, State18 /\ Msg18, State19 /\ Msg19, State20 /\ Msg20, State21 /\ Msg21, State22 /\ Msg22, State23 /\ Msg23, State24 /\ Msg24, State25 /\ Msg25, State26 /\ Msg26, State27 /\ Msg27, State28 /\ Msg28, State29 /\ Msg29, State30 /\ Msg30, State31 /\ Msg31, State32 /\ Msg32, State33 /\ Msg33, State34 /\ Msg34, State35 /\ Msg35, State36 /\ Msg36, State37 /\ Msg37, State38 /\ Msg38, State39 /\ Msg39, State40 /\ Msg40, State41 /\ Msg41, State42 /\ Msg42, State43 /\ Msg43, State44 /\ Msg44, State45 /\ Msg45, State46 /\ Msg46, State47 /\ Msg47, State48 /\ Msg48, State49 /\ Msg49, State50 /\ Msg50 ]

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