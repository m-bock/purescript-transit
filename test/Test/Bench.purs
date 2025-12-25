module Test.Bench (main) where

import Prelude

import BenchLib (bench, bench_, group, reportConsole, suite_)
import BenchLib as BenchLib
import BenchLib.Reporters.Json (reportJson_)
import BenchLib.Reporters.VegaLite (reportVegaLite)
import Data.Array (replicate)
import Data.Array as Array
import Data.Exists (Exists, mkExists)
import Data.Int as Int
import Data.List.Lazy as LazyList
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromJust)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Node.Process (lookupEnv)
import Partial.Unsafe (unsafeCrashWith)
import Test.BenchDef.Size10 as Size10
import Test.BenchDef.Size20 as Size20
--import Test.BenchDef.Size30 as Size30
import Test.BenchDef.Size40 as Size40
-- import Test.BenchDef.Size50 as Size50
-- import Test.BenchDef.Size60 as Size60
-- import Test.BenchDef.Size70 as Size70
-- import Test.BenchDef.Size80 as Size80
-- import Test.BenchDef.Size90 as Size90
-- import Test.BenchDef.Size100 as Size100
-- import Test.BenchDef.Size110 as Size110
-- import Test.BenchDef.Size120 as Size120
-- import Test.BenchDef.Size130 as Size130
-- import Test.BenchDef.Size140 as Size140
-- import Test.BenchDef.Size150 as Size150
-- import Test.BenchDef.Size160 as Size160
-- import Test.BenchDef.Size170 as Size170
-- import Test.BenchDef.Size180 as Size180
-- import Test.BenchDef.Size190 as Size190
-- import Test.BenchDef.Size200 as Size200
import Test.BenchDef.Transit (Msg, MsgD, State, StateD(..), printMsg, printMsgD, printState, printStateD, update, updateClassic, walk, walkD)
import Test.Exists2 (Exists2, mkExists2, runExists2)
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

type T = Unit -> Unit -> Array String

mk :: forall state msg. (state -> msg -> state) -> state -> Array msg -> (state -> String) -> (Unit -> Unit -> Array String)
mk update init msgs print = \_ ->
  let
    results = Array.scanl update init (msgs)
  in
    \_ -> map print results

inputsD :: Array (Int /\ (Unit -> Unit -> Array String))
inputsD =
  [ 10 /\ mk Size10.updateClassic (Size10.State01 {}) (map fst Size10.walkD) Size10.printStateD
  , 20 /\ mk Size20.updateClassic (Size20.State01 {}) (map fst Size20.walkD) Size20.printStateD
  --, 30 /\ mk Size30.updateClassic (Size30.State01 {}) (map fst Size30.walkD) Size30.printStateD
  , 40 /\ mk Size40.updateClassic (Size40.State01 {}) (map fst Size40.walkD) Size40.printStateD
  -- , 50 /\ mk Size50.updateClassic (Size50.State01 {}) (map fst Size50.walkD) Size50.printStateD
  -- , 60 /\ mk Size60.updateClassic (Size60.State01 {}) (map fst Size60.walkD) Size60.printStateD
  -- , 70 /\ mk Size70.updateClassic (Size70.State01 {}) (map fst Size70.walkD) Size70.printStateD
  -- , 80 /\ mk Size80.updateClassic (Size80.State01 {}) (map fst Size80.walkD) Size80.printStateD
  -- , 90 /\ mk Size90.updateClassic (Size90.State01 {}) (map fst Size90.walkD) Size90.printStateD
  -- , 100 /\ mk Size100.updateClassic (Size100.State01 {}) (map fst Size100.walkD) Size100.printStateD
  -- , 110 /\ mk Size110.updateClassic (Size110.State01 {}) (map fst Size110.walkD) Size110.printStateD
  -- , 120 /\ mk Size120.updateClassic (Size120.State01 {}) (map fst Size120.walkD) Size120.printStateD
  -- , 130 /\ mk Size130.updateClassic (Size130.State01 {}) (map fst Size130.walkD) Size130.printStateD
  -- , 140 /\ mk Size140.updateClassic (Size140.State01 {}) (map fst Size140.walkD) Size140.printStateD
  -- , 150 /\ mk Size150.updateClassic (Size150.State01 {}) (map fst Size150.walkD) Size150.printStateD
  -- , 160 /\ mk Size160.updateClassic (Size160.State01 {}) (map fst Size160.walkD) Size160.printStateD
  -- , 170 /\ mk Size170.updateClassic (Size170.State01 {}) (map fst Size170.walkD) Size170.printStateD
  -- , 180 /\ mk Size180.updateClassic (Size180.State01 {}) (map fst Size180.walkD) Size180.printStateD
  -- , 190 /\ mk Size190.updateClassic (Size190.State01 {}) (map fst Size190.walkD) Size190.printStateD
  -- , 200 /\ mk Size200.updateClassic (Size200.State01 {}) (map fst Size200.walkD) Size200.printStateD
  ]

inputs :: Array (Int /\ (Unit -> Unit -> Array String))
inputs =
  [ 10 /\ mk Size10.update (v @"State01" {}) (map fst Size10.walk) Size10.printState
  , 20 /\ mk Size20.update (v @"State01" {}) (map fst Size20.walk) Size20.printState
  --  , 30 /\ mk Size30.update (v @"State01" {}) (map fst Size30.walk) Size30.printState
  , 40 /\ mk Size40.update (v @"State01" {}) (map fst Size40.walk) Size40.printState
  -- , 50 /\ mk Size50.update (v @"State01" {}) (map fst Size50.walk) Size50.printState
  -- , 60 /\ mk Size60.update (v @"State01" {}) (map fst Size60.walk) Size60.printState
  -- , 70 /\ mk Size70.update (v @"State01" {}) (map fst Size70.walk) Size70.printState
  -- , 80 /\ mk Size80.update (v @"State01" {}) (map fst Size80.walk) Size80.printState
  -- , 90 /\ mk Size90.update (v @"State01" {}) (map fst Size90.walk) Size90.printState
  -- , 100 /\ mk Size100.update (v @"State01" {}) (map fst Size100.walk) Size100.printState
  -- , 110 /\ mk Size110.update (v @"State01" {}) (map fst Size110.walk) Size110.printState
  -- , 120 /\ mk Size120.update (v @"State01" {}) (map fst Size120.walk) Size120.printState
  -- , 130 /\ mk Size130.update (v @"State01" {}) (map fst Size130.walk) Size130.printState
  -- , 140 /\ mk Size140.update (v @"State01" {}) (map fst Size140.walk) Size140.printState
  -- , 150 /\ mk Size150.update (v @"State01" {}) (map fst Size150.walk) Size150.printState
  -- , 160 /\ mk Size160.update (v @"State01" {}) (map fst Size160.walk) Size160.printState
  -- , 170 /\ mk Size170.update (v @"State01" {}) (map fst Size170.walk) Size170.printState
  -- , 180 /\ mk Size180.update (v @"State01" {}) (map fst Size180.walk) Size180.printState
  -- , 190 /\ mk Size190.update (v @"State01" {}) (map fst Size190.walk) Size190.printState
  -- , 200 /\ mk Size200.update (v @"State01" {}) (map fst Size200.walk) Size200.printState
  ]

-- inputs :: Map Int BenchInput
-- inputs = Map.fromFoldable
--   [ 5 /\ (mkExists2 $ BenchInputF { msgs: map fst Size05.walk, update: Size05.update, init: v @"State01" {} })
--   , 10 /\ (mkExists2 $ BenchInputF { msgs: map fst Size10.walk, update: Size10.update, init: v @"State01" {} })
--   , 15 /\ (mkExists2 $ BenchInputF { msgs: map fst Size15.walk, update: Size15.update, init: v @"State01" {} })
--   , 20 /\ (mkExists2 $ BenchInputF { msgs: map fst Size20.walk, update: Size20.update, init: v @"State01" {} })
--   , 25 /\ (mkExists2 $ BenchInputF { msgs: map fst Size25.walk, update: Size25.update, init: v @"State01" {} })
--   , 30 /\ (mkExists2 $ BenchInputF { msgs: map fst Size30.walk, update: Size30.update, init: v @"State01" {} })
--   , 35 /\ (mkExists2 $ BenchInputF { msgs: map fst Size35.walk, update: Size35.update, init: v @"State01" {} })
--   , 40 /\ (mkExists2 $ BenchInputF { msgs: map fst Size40.walk, update: Size40.update, init: v @"State01" {} })
--   , 45 /\ (mkExists2 $ BenchInputF { msgs: map fst Size45.walk, update: Size45.update, init: v @"State01" {} })
--   , 50 /\ (mkExists2 $ BenchInputF { msgs: map fst Size50.walk, update: Size50.update, init: v @"State01" {} })
--   ]

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
            , sizes = map fst inputsD
            }
          [ bench
              "updateClassic"
              ( _
                  { normIn = \(s /\ _) -> s
                  , normOut = \f -> f unit
                  }
              )
              { prepare: \size -> case Array.find (\(s /\ _) -> s == size) inputsD of
                  Just input -> input
                  Nothing -> unsafeCrashWith "Input not found"
              , run: \(_ /\ f) -> f unit
              }
          , bench
              "update"
              ( _
                  { normIn = \(s /\ _) -> s
                  , normOut = \f -> f unit
                  }
              )
              { prepare: \size -> case Array.find (\(s /\ _) -> s == size) inputs of
                  Just input -> input
                  Nothing -> unsafeCrashWith "Input not found"
              , run: \(_ /\ f) -> f unit
              }
          ]
      -- , group "Update Functions"
      --     _
      --       { iterations = iterations
      --       , sizes = [ 50, 100, 200, 400, 800, 1600, 3200 ]
      --       }
      --     [ bench
      --         "updateClassic"
      --         ( _
      --             { normIn = map printMsgD
      --             , normOut = map printStateD
      --             }
      --         )
      --         { prepare: getInputsD
      --         , run: \msgs ->
      --             Array.scanl updateClassic (State01 {}) msgs
      --         }
      --     , bench
      --         "update"
      --         ( _
      --             { normIn = map printMsg
      --             , normOut = map printState
      --             }
      --         )
      --         { prepare: \size -> getInputs size
      --         , run: \msgs -> Array.scanl update (v @"State01") msgs
      --         }
      --     ]
      ]

showPad :: Int -> String
showPad n = if n < 10 then "0" <> show n else show n

getInputs :: Int -> Array Msg
getInputs n = Array.fromFoldable $ LazyList.take n $ LazyList.cycle $ LazyList.fromFoldable (map fst walk)

getOutputs :: Int -> Array State
getOutputs n = Array.fromFoldable $ LazyList.take n $ LazyList.cycle $ LazyList.fromFoldable (map snd walk)

getInputsD :: Int -> Array MsgD
getInputsD n = Array.fromFoldable $ LazyList.take n $ LazyList.cycle $ LazyList.fromFoldable (map fst walkD)

getOutputsD :: Int -> Array StateD
getOutputsD n = Array.fromFoldable $ LazyList.take n $ LazyList.cycle $ LazyList.fromFoldable (map snd walkD)