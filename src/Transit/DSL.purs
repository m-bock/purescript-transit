module Transit.DSL
  ( type (:*)
  , AddMatch
  , type (:?)
  , type (:@)
  , type (>|)
  , class FromDSL1
  , class FromDSL2
  , class FromDSL3
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

instance (FromDSL1 a a') => IsTransitSpec (Transit a) a'

class FromDSL1 :: forall k1 k2. k1 -> k2 -> Constraint
class FromDSL1 dsl a | dsl -> a

instance FromDSL1 Empty (C.MkTransitCore Nil')

else instance (FromDSL1 xs (C.MkTransitCore ys)) => FromDSL1 (Empty :* xs) (C.MkTransitCore (ys))

else instance (FromDSL2 x t, FromDSL1 xs (C.MkTransitCore ys)) => FromDSL1 (x :* xs) (C.MkTransitCore (t :> ys))

else instance (FromDSL2 x t) => FromDSL1 x (C.MkTransitCore (t :> Nil'))

class FromDSL2 :: forall k1 k2. k1 -> k2 -> Constraint
class FromDSL2 dsl a | dsl -> a

instance (FromDSL2 x (C.MkMatch s m xs), FromDSL3 y y') => FromDSL2 (x >| y) (C.MkMatch s m (y' :> xs))

instance FromDSL2 (x :@ y) (C.MkMatch x y Nil')

class FromDSL3 :: forall k1 k2. k1 -> k2 -> Constraint
class FromDSL3 dsl a | dsl -> a

instance FromDSL3 (g :? s) (C.MkReturnVia g s)

else instance FromDSL3 s (C.MkReturn s)

