module Transit.Class.FilterRow
  ( class FilterRow
  , filterRow
  ) where

import Prelude

import Data.Symbol (class IsSymbol)
import Data.Variant (Variant)
import Prim.Row as Row
import Transit.Core (MkReturn, MkReturnVia, Return, ReturnState, ReturnStateVia)
import Type.Data.List (type (:>), List', Nil')
import Unsafe.Coerce (unsafeCoerce)

class FilterRow (syms :: List' Return) (rin :: Row Type) (rout :: Row Type) (rout2 :: Row Type) | syms rin -> rout rout2 where
  filterRow :: Variant rout -> Variant rout2

instance FilterRow Nil' r () ()
  where
  filterRow = identity

instance
  ( Row.Cons symState (ReturnState a) rout' rout
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
  filterRow = unsafeCoerce -- "because no change of runtime representation, just unwrapping newtypes"

