module Transit.MatchSub where

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
import Transit.Core (MkReturn, MkReturnVia, RetVia, Return)
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
  GetSubset (syms :: List' Return) ty a
  | syms ty -> a
  where
  integrate :: a -> ty

type Opts = SingleField /\ ArgsToRecord NoTransform

instance
  ( GenericVariantLike Opts t r
  , FilterRow syms r r' r2'
  , Row.Union r2' rx r
  ) =>
  GetSubset syms (Generically t) (Variant r') where
  integrate v = Generically z
    where
    z :: t
    z = genericFromVariant (Proxy @Opts) $ y

    y :: Variant r
    y = V.expand x

    x :: Variant r2'
    x = f @syms @r v

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
  (Proxy :: _ (Variant ("Bar" :: RetVia "xxx" String)))

---

class FilterRow (syms :: List' Return) (rin :: Row Type) (rout :: Row Type) (rout2 :: Row Type) | syms rin -> rout rout2 where
  f :: Variant rout -> Variant rout2

instance FilterRow Nil' r () ()
  where
  f = identity

instance
  ( Row.Cons symState a rout' rout
  , Row.Cons symState a rout2' rout2
  , Row.Cons symState a rin' rin
  , FilterRow syms rin rout' rout2'
  ) =>
  FilterRow (MkReturn symState :> syms) rin rout rout2
  where
  f = unsafeCoerce

instance
  ( Row.Cons symState (RetVia symGuard a) rout' rout
  , Row.Cons symState a rout2' rout2
  , Row.Cons symState a rin' rin
  , FilterRow syms rin rout' rout2'
  , IsSymbol symState
  ) =>
  FilterRow (MkReturnVia symGuard symState :> syms) rin rout rout2
  where
  f v = unsafeCoerce "todo" --V.overOne (Proxy @symState) (unsafeCoerce :: RetVia symGuard a -> a) (?a) v
    where
    f' :: Variant rout' -> Variant rout2'
    f' = f @syms @rin

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
  (Proxy :: _ ("Bar" :: RetVia "xxx" String))
  (Proxy :: _ ("Bar" :: String))