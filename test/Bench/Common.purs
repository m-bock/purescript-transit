module Bench.Common (runBench, mkInput, Input, getConfigFromEnv) where

import Prelude

import BenchLib (bench, group, reportConsole, suite_)
import BenchLib as BenchLib
import BenchLib.Reporters.VegaLite (reportVegaLite)
import Data.Array as Array
import Data.Foldable (maximum, minimum)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Tuple (fst)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Node.Process (lookupEnv)
import Partial.Unsafe (unsafeCrashWith)

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

mkInput :: forall state msg. (state -> msg -> state) -> state -> Array msg -> (state -> String) -> Input
mkInput update init msgs print =
  let
    msgs' = join $ Array.replicate 100 msgs
  in
    \_ ->
      let
        result = Array.scanl update init msgs'
      in
        \_ -> map print result

type Input = Unit -> Unit -> Array String

unsafeFind :: forall a. Int -> Array (Int /\ a) -> (Int /\ a)
unsafeFind size items = case Array.find (\(s /\ _) -> s == size) items of
  Just item -> item
  Nothing -> unsafeCrashWith "Input not found"

runBench :: Config -> { inputs :: Array (Int /\ Input), inputsClassic :: Array (Int /\ Input) } -> Effect Unit
runBench { backend, iterations } { inputs, inputsClassic } =
  let
    allSizes = map fst inputs
  in
    BenchLib.runNode _
      { reporters =
          [ reportConsole
          , reportVegaLite _
              { folderPath = "bench/backend-" <> backend
              , minSize = minimum allSizes
              , maxSize = maximum allSizes
              }
          ]
      } $
      suite_
        ("Benchmarks for " <> backend <> " backend")
        [ group "Update Functions"
            _
              { iterations = iterations
              , sizes = allSizes
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

