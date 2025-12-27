module Test.BenchSmall (main) where

import Prelude

import Data.Tuple (fst)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Test.Bench (Input, getConfigFromEnv, mkInput, runBench)
import Test.BenchSmall.Classic.Size020 as ClassicSize20
import Test.BenchSmall.Classic.Size040 as ClassicSize40
import Test.BenchSmall.Classic.Size060 as ClassicSize60
import Test.BenchSmall.Classic.Size080 as ClassicSize80
import Test.BenchSmall.Classic.Size100 as ClassicSize100
import Test.BenchSmall.Transit.Size020 as TransitSize20
import Test.BenchSmall.Transit.Size040 as TransitSize40
import Test.BenchSmall.Transit.Size060 as TransitSize60
import Test.BenchSmall.Transit.Size080 as TransitSize80
import Test.BenchSmall.Transit.Size100 as TransitSize100

inputsClassic :: Array (Int /\ Input)
inputsClassic =
  [ 20 /\ mkInput ClassicSize20.updateClassic ClassicSize20.initClassic (map fst ClassicSize20.walkClassic) ClassicSize20.printStateClassic
  , 40 /\ mkInput ClassicSize40.updateClassic ClassicSize40.initClassic (map fst ClassicSize40.walkClassic) ClassicSize40.printStateClassic
  , 60 /\ mkInput ClassicSize60.updateClassic ClassicSize60.initClassic (map fst ClassicSize60.walkClassic) ClassicSize60.printStateClassic
  , 80 /\ mkInput ClassicSize80.updateClassic ClassicSize80.initClassic (map fst ClassicSize80.walkClassic) ClassicSize80.printStateClassic
  , 100 /\ mkInput ClassicSize100.updateClassic ClassicSize100.initClassic (map fst ClassicSize100.walkClassic) ClassicSize100.printStateClassic
  ]

inputs :: Array (Int /\ Input)
inputs =
  [ 20 /\ mkInput TransitSize20.update TransitSize20.init (map fst TransitSize20.walk) TransitSize20.printState
  , 40 /\ mkInput TransitSize40.update TransitSize40.init (map fst TransitSize40.walk) TransitSize40.printState
  , 60 /\ mkInput TransitSize60.update TransitSize60.init (map fst TransitSize60.walk) TransitSize60.printState
  , 80 /\ mkInput TransitSize80.update TransitSize80.init (map fst TransitSize80.walk) TransitSize80.printState
  , 100 /\ mkInput TransitSize100.update TransitSize100.init (map fst TransitSize100.walk) TransitSize100.printState
  ]

main :: Effect Unit
main = do
  config <- getConfigFromEnv

  runBench config { inputs, inputsClassic }
