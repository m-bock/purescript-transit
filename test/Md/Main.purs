module Md.Main where

import Prelude

import Effect (Effect)
import Patchdown as Patchdown

main :: Effect Unit
main = do
  Patchdown.main