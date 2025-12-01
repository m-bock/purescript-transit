module Transit.GetSubset where

import Prelude

import Data.Tuple.Nested (type (/\))
import Data.Variant (Variant)
import Data.Variant as V
import LabeledData.TransformEntry.Transforms (ArgsToRecord, NoTransform, SingleField)
import LabeledData.VariantLike.Generic (class GenericVariantLike, genericFromVariant)
import Prim.Row as Row
import Transit.Class.FilterRow (class FilterRow, filterRow)
import Transit.Core (Return)
import Transit.Util (Generically(..))
import Type.Data.List (List')
import Type.Proxy (Proxy(..))

---

class
  GetSubset (syms :: List' Return) ty a
  | syms ty -> a
  where
  getSubset :: a -> ty

type Opts = SingleField /\ ArgsToRecord NoTransform

instance
  ( GenericVariantLike Opts t r
  , FilterRow syms r r' r2'
  , Row.Union r2' rx r
  ) =>
  GetSubset syms (Generically t) (Variant r') where
  getSubset v = Generically z
    where
    z :: t
    z = genericFromVariant (Proxy @Opts) $ y

    y :: Variant r
    y = V.expand x

    x :: Variant r2'
    x = filterRow @syms @r v

