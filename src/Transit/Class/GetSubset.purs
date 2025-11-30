module Transit.GetSubset where

import Prelude

import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), NoArguments(..), Sum(..), from)
import Data.Symbol (class IsSymbol)
import Data.Tuple.Nested (type (/\))
import Data.Variant (Variant)
import Data.Variant as V
import LabeledData.TransformEntry.Transforms (ArgsToRecord, NoTransform, SingleField)
import LabeledData.VariantLike.Generic (class GenericVariantLike, genericFromVariant)
import Prim.Row as Row
import Safe.Coerce (coerce)
import Transit.Class.FilterRow (class FilterRow, filterRow)
import Transit.Core (MkReturn, MkReturnVia, Return, ReturnStateVia)
import Transit.Util (Generically(..))
import Type.Data.List (type (:>), List', Nil')
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

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

---

checkGetSubset :: forall syms t a. (GetSubset syms t a) => Proxy syms -> Proxy t -> Proxy a -> Unit
checkGetSubset _ _ _ = unit

data D
  = Foo Int
  | Bar String
  | Baz Boolean

derive instance Generic D _

test1 :: Unit
test1 = checkGetSubset
  (Proxy :: _ (MkReturn "Foo" :> Nil'))
  (Proxy :: _ (Generically D))
  (Proxy :: _ (Variant ("Foo" :: Int)))

test2 :: Unit
test2 = checkGetSubset
  (Proxy :: _ (MkReturnVia "xxx" "Bar" :> Nil'))
  (Proxy :: _ (Generically D))
  (Proxy :: _ (Variant ("Bar" :: ReturnStateVia "xxx" String)))

