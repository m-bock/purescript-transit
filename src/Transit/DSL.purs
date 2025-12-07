module Transit.DSL
  ( type (:*)
  , AddMatch
  , type (:?)
  , type (:@)
  , type (>|)
  , class ToTransitCore
  , class ToMatch
  , class ToReturn
  , Empty
  , Transit
  , StateWithMsg
  , WithGuard
  , AddOut
  ) where

import Data.Reflectable (class Reflectable, reflectType)
import Transit.Core (class IsTransitSpec, TransitCore_)
import Transit.Core as C
import Type.Data.List (type (:>), Nil')
import Type.Proxy (Proxy(..))

data AddMatch :: forall k1 k2. k1 -> k2 -> Type
data AddMatch a b

infixr 0 type AddMatch as :*

data StateWithMsg :: forall k1 k2. k1 -> k2 -> Type
data StateWithMsg a b

infixl 5 type StateWithMsg as :@

data AddOut :: forall k1 k2. k1 -> k2 -> Type
data AddOut a b

infixl 5 type AddOut as >|

data WithGuard :: forall k1 k2. k1 -> k2 -> Type
data WithGuard a b

infixl 9 type WithGuard as :?

---

instance (IsTransitSpec (Transit dsl) o, Reflectable o TransitCore_) => Reflectable (Transit dsl) TransitCore_ where
  reflectType _ = reflectType (Proxy @o)

---

data Transit :: forall k. k -> Type
data Transit a

data Empty

instance (ToTransitCore a a') => IsTransitSpec (Transit a) a'

class ToTransitCore :: forall k. k -> C.TransitCore -> Constraint
class ToTransitCore dsl a | dsl -> a

instance ToTransitCore Empty (C.MkTransitCore Nil')

else instance (ToTransitCore xs (C.MkTransitCore ys)) => ToTransitCore (Empty :* xs) (C.MkTransitCore (ys))

else instance (ToMatch x t, ToTransitCore xs (C.MkTransitCore ys)) => ToTransitCore (x :* xs) (C.MkTransitCore (t :> ys))

else instance (ToMatch x t) => ToTransitCore x (C.MkTransitCore (t :> Nil'))

class ToMatch :: forall k. k -> C.Match -> Constraint
class ToMatch dsl a | dsl -> a

instance (ToMatch x (C.MkMatch s m xs), ToReturn y y') => ToMatch (x >| y) (C.MkMatch s m (y' :> xs))

instance ToMatch (x :@ y) (C.MkMatch x y Nil')

class ToReturn :: forall k. k -> C.Return -> Constraint
class ToReturn dsl a | dsl -> a

instance ToReturn (g :? s) (C.MkReturnVia g s)

else instance ToReturn s (C.MkReturn s)

