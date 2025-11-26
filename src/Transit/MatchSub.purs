module Transit.MatchSub where

import Prelude

import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), NoArguments(..), Sum(..), from, to)
import Data.Tuple.Nested (type (/\))
import Data.Variant (Variant)
import Data.Variant as V
import LabeledData.TransformEntry.Transforms (ArgsToRecord, NoTransform, SingleField)
import LabeledData.VariantLike.Generic (class GenericVariantLike, genericFromVariant)
import Prim.Row as Row
import Transit.Util (Generically(..))
import Type.Data.List (type (:>), List', Nil')
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

class
  MatchSub (sym :: Symbol) ty a
  | sym ty -> a
  where
  matchSub :: forall z. (a -> z) -> (Unit -> z) -> ty -> z

matchSub2 :: forall @sym1 @sym2 a1 b1 a2 b2 z. MatchSub sym1 a1 b1 => MatchSub sym2 a2 b2 => (b1 -> b2 -> z) -> (Unit -> z) -> a1 -> a2 -> z
matchSub2 f z x1 x2 =
  matchSub @sym1 (\y1 -> matchSub @sym2 (\y2 -> f y1 y2) z x2) z x1

instance (Generic ty rep, MatchSub sym rep a) => MatchSub sym (Generically ty) a
  where
  matchSub onMatch onDefault (Generically x) = matchSub @sym onMatch onDefault $ from x

instance MatchSub sym (Constructor sym (Argument a)) a where
  matchSub onMatch _ (Constructor (Argument x)) = onMatch x

else instance MatchSub sym (Constructor sym NoArguments) Unit where
  matchSub onMatch _ (Constructor NoArguments) = onMatch unit

else instance MatchSub sym (Constructor sym2 b) a where
  matchSub _ onDefault _ = onDefault unit

instance (MatchSub sym t1 a, MatchSub sym t2 a) => MatchSub sym (Sum t1 t2) a where
  matchSub onMatch onDefault (Inl x) = matchSub @sym onMatch onDefault x
  matchSub onMatch onDefault (Inr x) = matchSub @sym onMatch onDefault x

---

class
  GetSubset (syms :: List' Symbol) ty a
  | syms ty -> a
  where
  integrate :: a -> ty

type Opts = SingleField /\ ArgsToRecord NoTransform

instance (GenericVariantLike Opts t r, FilterRow syms r r', Row.Union r' rx r) => GetSubset syms (Generically t) (Variant r') where
  integrate v = Generically $ genericFromVariant (Proxy @Opts) $ V.expand v

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
  (Proxy :: _ ("Foo" :> Nil'))
  (Proxy :: _ (Generically D))
  (Proxy :: _ (Variant ("Foo" :: Int)))

test2 :: Unit
test2 = checkGetSubset
  (Proxy :: _ ("Bar" :> "Baz" :> Nil'))
  (Proxy :: _ (Generically D))
  (Proxy :: _ (Variant ("Bar" :: String, "Baz" :: Boolean)))

---

class FilterRow (syms :: List' Symbol) (rin :: Row Type) (rout :: Row Type) | syms rin -> rout

instance FilterRow Nil' r ()

instance
  ( Row.Cons sym a rout' rout
  , FilterRow syms rin rout'
  ) =>
  FilterRow (sym :> syms) rin rout