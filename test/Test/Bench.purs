module Test.Bench where

import Prelude

import BenchLib (bench_, group, group_, suite_)
import BenchLib as BenchLib
import BenchLib.Reporters.Html (reportHtml)
import Data.Array (foldl)
import Data.Array as Array
import Data.List.Lazy as LazyList
import Effect (Effect)
import Test.Examples.DoorWithPin (Msg(..), State(..), update, updateClassic)
import Test.Examples.Variants as V
import Type.Proxy (Proxy(..))

initState :: State
initState = DoorOpen

msgs :: Int -> Array Msg
msgs n = join $ Array.replicate n
  [ Close
  , Open
  , Close
  , Lock { newPin: "1234" }
  , Unlock { enteredPin: "abcd" }
  , Unlock { enteredPin: "1234" }
  , Open
  ]

initStateV :: V.State
initStateV = V.inj @"DoorOpen" unit

msgsV :: Int -> Array V.Msg
msgsV n = join $ Array.replicate n
  [ V.inj @"Close" unit
  , V.inj @"Open" unit
  , V.inj @"Close" unit
  , V.inj @"Lock" { newPin: "1234" }
  , V.inj @"Unlock" { enteredPin: "abcd" }
  , V.inj @"Unlock" { enteredPin: "1234" }
  , V.inj @"Open" unit
  ]

main :: Effect Unit
main =
  BenchLib.runNode _
    { reporters =
        [ reportHtml \cfg -> cfg
            { filePath = "report.html"
            }
        ]
    } $
    suite_
      "Minimal Example"
      [ group "Replicate Functions"
          _ { iterations = 1000000, sizes = [ 1, 10, 100, 1000 ] }
          [ bench_
              "update"
              { prepare: msgs
              , run: \xs -> foldl update initState xs
              }
          , bench_
              "updateClassic"
              { prepare: msgs
              , run: \xs -> foldl updateClassic initState xs
              }
          , bench_
              "V.update"
              { prepare: msgsV
              , run: \xs -> foldl V.update initStateV xs
              }
          ]
      ]