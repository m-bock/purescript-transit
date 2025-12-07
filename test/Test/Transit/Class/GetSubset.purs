module Test.Transit.Class.GetSubset
  ( check
  , test1
  , test2
  ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Variant (Variant)
import Transit.Core (MkReturn, MkReturnVia, ReturnState, ReturnStateVia)
import Transit.Class.GetSubset (class GetSubset)
import Transit.Util (Generically)
import Type.Data.List (Cons', Nil')

check :: forall @syms @t @a. (GetSubset syms t a) => Unit
check = unit

data D
  = Foo Int
  | Bar String
  | Baz

derive instance Generic D _

test1 :: Unit
test1 = check
  @(Nil')
  @(Generically D)
  @(Variant ())

test2 :: Unit
test2 = check
  @(Cons' (MkReturn "Foo") (Cons' (MkReturnVia "Tansition" "Bar") Nil'))
  @(Generically D)
  @(Variant ("Foo" :: ReturnState Int, "Bar" :: ReturnStateVia "Tansition" String))
