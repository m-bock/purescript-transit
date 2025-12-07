module Test.Transit.Class.FilterRow
  ( check
  , test1
  , test2
  ) where

import Prelude

import Transit.Class.FilterRow (class FilterRow)
import Transit.Core (MkReturn, MkReturnVia, ReturnState, ReturnStateVia)
import Type.Data.List (Cons', Nil')

check :: forall @syms @rin @rout @rout2. (FilterRow syms rin rout rout2) => Unit
check = unit

test1 :: Unit
test1 = check
  @(Nil')
  @("Foo" :: Int, "Bar" :: String, "Baz" :: Boolean)
  @()
  @()

test2 :: Unit
test2 = check
  @(Cons' (MkReturn "Foo") (Cons' (MkReturnVia "Transition" "Bar") Nil'))
  @("Foo" :: Int, "Bar" :: String, "Baz" :: Boolean)
  @("Foo" :: ReturnState Int, "Bar" :: ReturnStateVia "Transition" String)
  @("Foo" :: Int, "Bar" :: String)