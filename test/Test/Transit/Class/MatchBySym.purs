module Test.Transit.Class.MatchBySym
  ( check
  , test1
  , test2
  , test3
  ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Transit.Class.MatchBySym (class MatchBySym)
import Transit.Util (Generically)

check :: forall @sym @a @b. MatchBySym sym a b => Unit
check = unit

data Test = Foo Int | Bar String | Baz Boolean

derive instance Generic Test _

test1 :: Unit
test1 = check @"Foo" @(Generically Test) @Int

test2 :: Unit
test2 = check @"Bar" @(Generically Test) @String

test3 :: Unit
test3 = check @"Baz" @(Generically Test) @Boolean