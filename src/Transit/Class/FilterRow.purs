module Transit.Class.FilterRow where

import Prelude

import Data.Symbol (class IsSymbol)
import Data.Variant (Variant)
import Prim.Row as Row
import Transit.Core (MkReturn, MkReturnVia, Return, ReturnStateVia)
import Type.Data.List (type (:>), List', Nil')
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

---

class FilterRow (syms :: List' Return) (rin :: Row Type) (rout :: Row Type) (rout2 :: Row Type) | syms rin -> rout rout2 where
  filterRow :: Variant rout -> Variant rout2

instance FilterRow Nil' r () ()
  where
  filterRow = identity

instance
  ( Row.Cons symState a rout' rout
  , Row.Cons symState a rout2' rout2
  , Row.Cons symState a rin' rin
  , FilterRow syms rin rout' rout2'
  ) =>
  FilterRow (MkReturn symState :> syms) rin rout rout2
  where
  filterRow = unsafeCoerce

instance
  ( Row.Cons symState (ReturnStateVia symGuard a) rout' rout
  , Row.Cons symState a rout2' rout2
  , Row.Cons symState a rin' rin
  , FilterRow syms rin rout' rout2'
  , IsSymbol symState
  ) =>
  FilterRow (MkReturnVia symGuard symState :> syms) rin rout rout2
  where
  filterRow v = unsafeCoerce "todo" --V.overOne (Proxy @symState) (unsafeCoerce :: RetVia symGuard a -> a) (?a) v

checkFilterRow :: forall syms rin rout rout2. (FilterRow syms rin rout rout2) => Proxy syms -> Proxy rin -> Proxy rout -> Proxy rout2 -> Unit
checkFilterRow _ _ _ _ = unit

test3 :: Unit
test3 = checkFilterRow
  (Proxy :: _ (MkReturn "Foo" :> Nil'))
  (Proxy :: _ ("Foo" :: Int))
  (Proxy :: _ ("Foo" :: Int))
  (Proxy :: _ ("Foo" :: Int))

test4 :: Unit
test4 = checkFilterRow
  (Proxy :: _ (MkReturnVia "xxx" "Bar" :> Nil'))
  (Proxy :: _ ("Bar" :: String))
  (Proxy :: _ ("Bar" :: ReturnStateVia "xxx" String))
  (Proxy :: _ ("Bar" :: String))