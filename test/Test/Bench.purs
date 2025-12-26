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
import Test.BenchDef.TransitSize10 as TransitSize10
import Test.BenchDef.TransitSize20 as TransitSize20
import Test.BenchDef.TransitSize30 as TransitSize30
import Test.BenchDef.TransitSize40 as TransitSize40
import Test.BenchDef.TransitSize50 as TransitSize50
import Test.BenchDef.TransitSize60 as TransitSize60
import Test.BenchDef.TransitSize70 as TransitSize70
import Test.BenchDef.TransitSize80 as TransitSize80
import Test.BenchDef.TransitSize90 as TransitSize90
import Test.BenchDef.ClassicSize10 as ClassicSize10
import Test.BenchDef.ClassicSize20 as ClassicSize20
import Test.BenchDef.ClassicSize30 as ClassicSize30
import Test.BenchDef.ClassicSize40 as ClassicSize40
import Test.BenchDef.ClassicSize50 as ClassicSize50
import Test.BenchDef.ClassicSize60 as ClassicSize60
import Test.BenchDef.ClassicSize70 as ClassicSize70
import Test.BenchDef.ClassicSize80 as ClassicSize80
import Test.BenchDef.ClassicSize90 as ClassicSize90

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
    result = Array.scanl update init msgs
  in
    \_ -> map print result

inputsClassic :: Array (Int /\ (Unit -> Unit -> Array String))
inputsClassic =
  [ 10 /\ mk ClassicSize10.updateClassic ClassicSize10.initClassic (map fst ClassicSize10.walkClassic) ClassicSize10.printStateClassic
  , 20 /\ mk ClassicSize20.updateClassic ClassicSize20.initClassic (map fst ClassicSize20.walkClassic) ClassicSize20.printStateClassic
  , 30 /\ mk ClassicSize30.updateClassic ClassicSize30.initClassic (map fst ClassicSize30.walkClassic) ClassicSize30.printStateClassic
  , 40 /\ mk ClassicSize40.updateClassic ClassicSize40.initClassic (map fst ClassicSize40.walkClassic) ClassicSize40.printStateClassic
  , 50 /\ mk ClassicSize50.updateClassic ClassicSize50.initClassic (map fst ClassicSize50.walkClassic) ClassicSize50.printStateClassic
  , 60 /\ mk ClassicSize60.updateClassic ClassicSize60.initClassic (map fst ClassicSize60.walkClassic) ClassicSize60.printStateClassic
  , 70 /\ mk ClassicSize70.updateClassic ClassicSize70.initClassic (map fst ClassicSize70.walkClassic) ClassicSize70.printStateClassic
  , 80 /\ mk ClassicSize80.updateClassic ClassicSize80.initClassic (map fst ClassicSize80.walkClassic) ClassicSize80.printStateClassic
  , 90 /\ mk ClassicSize90.updateClassic ClassicSize90.initClassic (map fst ClassicSize90.walkClassic) ClassicSize90.printStateClassic
  ]

inputs :: Array (Int /\ (Unit -> Unit -> Array String))
inputs =
  [ 10 /\ mk TransitSize10.update TransitSize10.init (map fst TransitSize10.walk) TransitSize10.printState
  , 20 /\ mk TransitSize20.update TransitSize20.init (map fst TransitSize20.walk) TransitSize20.printState
  , 30 /\ mk TransitSize30.update TransitSize30.init (map fst TransitSize30.walk) TransitSize30.printState
  , 40 /\ mk TransitSize40.update TransitSize40.init (map fst TransitSize40.walk) TransitSize40.printState
  , 50 /\ mk TransitSize50.update TransitSize50.init (map fst TransitSize50.walk) TransitSize50.printState
  , 60 /\ mk TransitSize60.update TransitSize60.init (map fst TransitSize60.walk) TransitSize60.printState
  , 70 /\ mk TransitSize70.update TransitSize70.init (map fst TransitSize70.walk) TransitSize70.printState
  , 80 /\ mk TransitSize80.update TransitSize80.init (map fst TransitSize80.walk) TransitSize80.printState
  , 90 /\ mk TransitSize90.update TransitSize90.init (map fst TransitSize90.walk) TransitSize90.printState
  ]

unsafeFind :: forall a. Int -> Array (Int /\ a) -> (Int /\ a)
unsafeFind size items = case Array.find (\(s /\ _) -> s == size) items of
  Just item -> item
  Nothing -> unsafeCrashWith "Input not found"

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
            , sizes = [ 10, 20, 30, 40, 50, 60, 70, 80, 90 ]
            }
          [ bench
              "updateClassic"
              ( _
                  { normIn = \(s /\ _) -> s
                  , normOut = \f -> f unit
                  }
              )
              { prepare: \size -> unsafeFind size inputsClassic
              , run: \(_ /\ f) -> f unit
              }
          , bench
              "update"
              ( _
                  { normIn = \(s /\ _) -> s
                  , normOut = \f -> f unit
                  }
              )
              { prepare: \size -> unsafeFind size inputs
              , run: \(_ /\ f) -> f unit
              }
          ]
      ]

