module Transit.Class.GetSubset
  ( class GetSubset
  , getSubset
  ) where

import Data.Variant (Variant)
import Data.Variant as V
import Prim.Row as Row
import Transit.Class.FilterRow (class FilterRow, filterRow)
import Transit.Core (ReturnTL)
import Type.Data.List (List')

class
  GetSubset (syms :: List' ReturnTL) ty a
  | syms ty -> a
  where
  getSubset :: a -> ty

instance
  ( FilterRow syms r r' r2'
  , Row.Union r2' rx r
  ) =>
  GetSubset syms (Variant r) (Variant r') where
  getSubset v = y
    where
    y :: Variant r
    y = V.expand x

    x :: Variant r2'
    x = filterRow @syms @r v
