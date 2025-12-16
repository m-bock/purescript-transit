module Test.Bench where

import Prelude

import BenchLib (bench_, group, group_, suite_)
import BenchLib as BenchLib
import BenchLib.Reporters.Html (reportHtml)
import BenchLib.Reporters.Markdown (reportMarkdown_)
import Data.Array (foldl)
import Data.Array as Array
import Data.Generic.Rep (class Generic)
import Data.List.Lazy as LazyList
import Data.Maybe (fromMaybe)
import Data.Tuple.Nested ((/\))
import Data.Variant (Variant)
import Data.Variant as V
import Effect (Effect)
import Node.Process (lookupEnv)
import Partial.Unsafe (unsafeCrashWith)
import Transit (type (:*), type (:@), type (>|), Empty, Transit, match, mkUpdate, mkUpdateGeneric, return)
import Type.Function (type ($))
import Type.Proxy (Proxy(..))

data State
  = State01
  | State02
  | State03
  | State04
  | State05
  | State06
  | State07
  | State08
  | State09
  | State10
  | State11
  | State12
  | State13
  | State14
  | State15
  | State16
  | State17
  | State18
  | State19
  | State20

derive instance Generic State _

data Msg
  = Msg01
  | Msg02
  | Msg03
  | Msg04
  | Msg05
  | Msg06
  | Msg07
  | Msg08
  | Msg09
  | Msg10
  | Msg11
  | Msg12
  | Msg13
  | Msg14
  | Msg15
  | Msg16
  | Msg17
  | Msg18
  | Msg19
  | Msg20

derive instance Generic Msg _

type StateV = Variant
  ( "State01" :: Unit
  , "State02" :: Unit
  , "State03" :: Unit
  , "State04" :: Unit
  , "State05" :: Unit
  , "State06" :: Unit
  , "State07" :: Unit
  , "State08" :: Unit
  , "State09" :: Unit
  , "State10" :: Unit
  , "State11" :: Unit
  , "State12" :: Unit
  , "State13" :: Unit
  , "State14" :: Unit
  , "State15" :: Unit
  , "State16" :: Unit
  , "State17" :: Unit
  , "State18" :: Unit
  , "State19" :: Unit
  , "State20" :: Unit
  )

type MsgV = Variant
  ( "Msg01" :: Unit
  , "Msg02" :: Unit
  , "Msg03" :: Unit
  , "Msg04" :: Unit
  , "Msg05" :: Unit
  , "Msg06" :: Unit
  , "Msg07" :: Unit
  , "Msg08" :: Unit
  , "Msg09" :: Unit
  , "Msg10" :: Unit
  , "Msg11" :: Unit
  , "Msg12" :: Unit
  , "Msg13" :: Unit
  , "Msg14" :: Unit
  , "Msg15" :: Unit
  , "Msg16" :: Unit
  , "Msg17" :: Unit
  , "Msg18" :: Unit
  , "Msg19" :: Unit
  , "Msg20" :: Unit
  )

updateClassic :: State -> Msg -> State
updateClassic state msg = case state, msg of
  State01, Msg01 -> State02
  State02, Msg02 -> State03
  State03, Msg03 -> State04
  State04, Msg04 -> State05
  State05, Msg05 -> State06
  State06, Msg06 -> State07
  State07, Msg07 -> State08
  State08, Msg08 -> State09
  State09, Msg09 -> State10
  State10, Msg10 -> State11
  State11, Msg11 -> State12
  State12, Msg12 -> State13
  State13, Msg13 -> State14
  State14, Msg14 -> State15
  State15, Msg15 -> State16
  State16, Msg16 -> State17
  State17, Msg17 -> State18
  State18, Msg18 -> State19
  State19, Msg19 -> State20
  State20, Msg20 -> State01
  _, _ -> state

type Size5Transit =
  Transit $ Empty
    :* ("State01" :@ "Msg01" >| "State02")
    :* ("State02" :@ "Msg02" >| "State03")
    :* ("State03" :@ "Msg03" >| "State04")
    :* ("State04" :@ "Msg04" >| "State05")
    :* ("State05" :@ "Msg05" >| "State06")
    :* ("State06" :@ "Msg06" >| "State07")
    :* ("State07" :@ "Msg07" >| "State08")
    :* ("State08" :@ "Msg08" >| "State09")
    :* ("State09" :@ "Msg09" >| "State10")
    :* ("State10" :@ "Msg10" >| "State11")
    :* ("State11" :@ "Msg11" >| "State12")
    :* ("State12" :@ "Msg12" >| "State13")
    :* ("State13" :@ "Msg13" >| "State14")
    :* ("State14" :@ "Msg14" >| "State15")
    :* ("State15" :@ "Msg15" >| "State16")
    :* ("State16" :@ "Msg16" >| "State17")
    :* ("State17" :@ "Msg17" >| "State18")
    :* ("State18" :@ "Msg18" >| "State19")
    :* ("State19" :@ "Msg19" >| "State20")
    :* ("State20" :@ "Msg20" >| "State01")

update :: State -> Msg -> State
update = mkUpdateGeneric @Size5Transit
  (match @"State01" @"Msg01" \_ _ -> return @"State02")
  (match @"State02" @"Msg02" \_ _ -> return @"State03")
  (match @"State03" @"Msg03" \_ _ -> return @"State04")
  (match @"State04" @"Msg04" \_ _ -> return @"State05")
  (match @"State05" @"Msg05" \_ _ -> return @"State06")
  (match @"State06" @"Msg06" \_ _ -> return @"State07")
  (match @"State07" @"Msg07" \_ _ -> return @"State08")
  (match @"State08" @"Msg08" \_ _ -> return @"State09")
  (match @"State09" @"Msg09" \_ _ -> return @"State10")
  (match @"State10" @"Msg10" \_ _ -> return @"State11")
  (match @"State11" @"Msg11" \_ _ -> return @"State12")
  (match @"State12" @"Msg12" \_ _ -> return @"State13")
  (match @"State13" @"Msg13" \_ _ -> return @"State14")
  (match @"State14" @"Msg14" \_ _ -> return @"State15")
  (match @"State15" @"Msg15" \_ _ -> return @"State16")
  (match @"State16" @"Msg16" \_ _ -> return @"State17")
  (match @"State17" @"Msg17" \_ _ -> return @"State18")
  (match @"State18" @"Msg18" \_ _ -> return @"State19")
  (match @"State19" @"Msg19" \_ _ -> return @"State20")
  (match @"State20" @"Msg20" \_ _ -> return @"State01")

updateV :: StateV -> MsgV -> StateV
updateV = mkUpdate @Size5Transit
  (match @"State01" @"Msg01" \_ _ -> return @"State02")
  (match @"State02" @"Msg02" \_ _ -> return @"State03")
  (match @"State03" @"Msg03" \_ _ -> return @"State04")
  (match @"State04" @"Msg04" \_ _ -> return @"State05")
  (match @"State05" @"Msg05" \_ _ -> return @"State06")
  (match @"State06" @"Msg06" \_ _ -> return @"State07")
  (match @"State07" @"Msg07" \_ _ -> return @"State08")
  (match @"State08" @"Msg08" \_ _ -> return @"State09")
  (match @"State09" @"Msg09" \_ _ -> return @"State10")
  (match @"State10" @"Msg10" \_ _ -> return @"State11")
  (match @"State11" @"Msg11" \_ _ -> return @"State12")
  (match @"State12" @"Msg12" \_ _ -> return @"State13")
  (match @"State13" @"Msg13" \_ _ -> return @"State14")
  (match @"State14" @"Msg14" \_ _ -> return @"State15")
  (match @"State15" @"Msg15" \_ _ -> return @"State16")
  (match @"State16" @"Msg16" \_ _ -> return @"State17")
  (match @"State17" @"Msg17" \_ _ -> return @"State18")
  (match @"State18" @"Msg18" \_ _ -> return @"State19")
  (match @"State19" @"Msg19" \_ _ -> return @"State20")
  (match @"State20" @"Msg20" \_ _ -> return @"State01")

main :: Effect Unit
main = do
  be <- lookupEnv "BACKEND" <#> fromMaybe "JS"

  BenchLib.runNode _
    { reporters =
        [ reportHtml \cfg -> cfg
            { filePath = "report-" <> be <> ".html"
            }
        , reportMarkdown_
        ]
    } $
    suite_
      ("Benchmarks for " <> be <> " backend")
      [ group "Update Functions"
          _ { iterations = 100000, sizes = [ 5, 10, 15, 20 ] }
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