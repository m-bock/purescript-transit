module Test.Bench (main) where

import Prelude

import BenchLib (bench, group, reportConsole, suite_)
import BenchLib as BenchLib
import BenchLib.Reporters.Json (reportJson_)
import BenchLib.Reporters.VegaLite (reportVegaLite)
import Data.Array as Array
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Tuple (fst)
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
import Test.BenchDef.TransitSize100 as TransitSize100
import Test.BenchDef.TransitSize110 as TransitSize110
import Test.BenchDef.TransitSize120 as TransitSize120
import Test.BenchDef.TransitSize130 as TransitSize130
import Test.BenchDef.TransitSize140 as TransitSize140
import Test.BenchDef.TransitSize150 as TransitSize150
import Test.BenchDef.ClassicSize10 as ClassicSize10
import Test.BenchDef.ClassicSize20 as ClassicSize20
import Test.BenchDef.ClassicSize30 as ClassicSize30
import Test.BenchDef.ClassicSize40 as ClassicSize40
import Test.BenchDef.ClassicSize50 as ClassicSize50
import Test.BenchDef.ClassicSize60 as ClassicSize60
import Test.BenchDef.ClassicSize70 as ClassicSize70
import Test.BenchDef.ClassicSize80 as ClassicSize80
import Test.BenchDef.ClassicSize90 as ClassicSize90
import Test.BenchDef.ClassicSize100 as ClassicSize100
import Test.BenchDef.ClassicSize110 as ClassicSize110
import Test.BenchDef.ClassicSize120 as ClassicSize120
import Test.BenchDef.ClassicSize130 as ClassicSize130
import Test.BenchDef.ClassicSize140 as ClassicSize140
import Test.BenchDef.ClassicSize150 as ClassicSize150

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
  , 100 /\ mk ClassicSize100.updateClassic ClassicSize100.initClassic (map fst ClassicSize100.walkClassic) ClassicSize100.printStateClassic
  , 110 /\ mk ClassicSize110.updateClassic ClassicSize110.initClassic (map fst ClassicSize110.walkClassic) ClassicSize110.printStateClassic
  , 120 /\ mk ClassicSize120.updateClassic ClassicSize120.initClassic (map fst ClassicSize120.walkClassic) ClassicSize120.printStateClassic
  , 130 /\ mk ClassicSize130.updateClassic ClassicSize130.initClassic (map fst ClassicSize130.walkClassic) ClassicSize130.printStateClassic
  , 140 /\ mk ClassicSize140.updateClassic ClassicSize140.initClassic (map fst ClassicSize140.walkClassic) ClassicSize140.printStateClassic
  , 150 /\ mk ClassicSize150.updateClassic ClassicSize150.initClassic (map fst ClassicSize150.walkClassic) ClassicSize150.printStateClassic
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
  , 100 /\ mk TransitSize100.update TransitSize100.init (map fst TransitSize100.walk) TransitSize100.printState
  , 110 /\ mk TransitSize110.update TransitSize110.init (map fst TransitSize110.walk) TransitSize110.printState
  , 120 /\ mk TransitSize120.update TransitSize120.init (map fst TransitSize120.walk) TransitSize120.printState
  , 130 /\ mk TransitSize130.update TransitSize130.init (map fst TransitSize130.walk) TransitSize130.printState
  , 140 /\ mk TransitSize140.update TransitSize140.init (map fst TransitSize140.walk) TransitSize140.printState
  , 150 /\ mk TransitSize150.update TransitSize150.init (map fst TransitSize150.walk) TransitSize150.printState
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
            , sizes = [ 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120, 130, 140, 150 ]
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

