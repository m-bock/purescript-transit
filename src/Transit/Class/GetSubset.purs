module Transit.GetSubset
  ( class GetSubset
  , getSubset
  , SingleOrNoField
  , Opts
  , class Util
  , getField
  , getField'
  ) where

import Prelude

import Data.Tuple.Nested (type (/\))
import Data.Variant (Variant)
import Data.Variant as V
import LabeledData.TransformEntry (class TransformEntry)
import LabeledData.TransformEntry.Transforms (ArgsToRecord, NoTransform, SingleField)
import LabeledData.VariantLike.Generic (class GenericVariantLike, genericFromVariant)
import Prim.Row as Row
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RL
import Record as Record
import Transit.Class.FilterRow (class FilterRow, filterRow)
import Transit.Core (Return)
import Transit.Util (Generically(..))
import Type.Data.List (List')
import Type.Equality (class TypeEquals)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

---

class
  GetSubset (syms :: List' Return) ty a
  | syms ty -> a
  where
  getSubset :: a -> ty

---

data SingleOrNoField

instance
  ( Util rl r a
  , RowToList r rl
  ) =>
  TransformEntry SingleOrNoField sym sym (Record r) a where
  transformEntry _ _ _ = getField @rl
  untransformEntry _ _ _ x = getField' @rl x

class Util (rl :: RowList Type) (r :: Row Type) a | rl -> r a where
  getField :: Record r -> a
  getField' :: a -> Record r

instance Util RL.Nil () Unit where
  getField _ = unit
  getField' _ = {}

instance Util (RL.Cons "1" a RL.Nil) ("1" :: a) a where
  getField r = Record.get (Proxy :: _ "1") r
  getField' x = Record.insert (Proxy :: _ "1") x {}

---

type Opts = SingleOrNoField /\ ArgsToRecord NoTransform

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

