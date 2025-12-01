module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console (log)
import Patchdown as Patchdown
import Test.Example as Example

main :: Effect Unit
main = do
  Example.main
  Patchdown.main