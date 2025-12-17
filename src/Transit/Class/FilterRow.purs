module Transit.Class.FilterRow
  ( class FilterRow
  , filterRow
  ) where

import Prelude

import Data.Symbol (class IsSymbol)
import Data.Variant (Variant)
import Prim.Row as Row
import Transit.Core (MkReturnTL, MkReturnViaTL, ReturnTL, ReturnState, Via)
import Type.Data.List (type (:>), List', Nil')
import Unsafe.Coerce (unsafeCoerce)

class FilterRow (syms :: List' ReturnTL) (rin :: Row Type) (rout :: Row Type) (rout2 :: Row Type) | syms rin -> rout rout2 where
  filterRow :: Variant rout -> Variant rout2

instance filterRowNil :: FilterRow Nil' r () ()
  where
  filterRow = identity

instance filterRowConsState ::
  ( Row.Cons symState a rout' rout
  , Row.Cons symState a rout2' rout2
  , Row.Cons symState a rin' rin
  , FilterRow syms rin rout' rout2'
  ) =>
  FilterRow (MkReturnTL symState :> syms) rin rout rout2
  where
  filterRow = unsafeCoerce

instance filterRowConsStateVia ::
  ( Row.Cons symState (Via symGuard a) rout' rout
  , Row.Cons symState a rout2' rout2
  , Row.Cons symState a rin' rin
  , FilterRow syms rin rout' rout2'
  , IsSymbol symState
  ) =>
  FilterRow (MkReturnViaTL symGuard symState :> syms) rin rout rout2
  where
  filterRow = unsafeCoerce -- "because no change of runtime representation, just unwrapping newtypes"

