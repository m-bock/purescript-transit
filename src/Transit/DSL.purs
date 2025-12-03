module Transit.DSL where

import Data.Reflectable (class Reflectable, reflectType)
import Data.Unit (Unit, unit)
import Transit.Core (MkReturn, MkReturnVia, StateGraph, StateGraph_(..))
import Transit.Core as C
import Transit.Util (type (:<))
import Type.Data.List (type (:>), Cons', Nil')
import Type.Proxy (Proxy(..))

foreign import data StateSpec :: Type

--foreign import data MkStateSpec :: forall k. k -> StateSpec

data C

data D a b

infixr 0 type D as :*

data StateWithMsg a b

infixl 5 type StateWithMsg as :@

data AddOut a b

infixl 5 type AddOut as >|

data J a b

infixl 9 type J as :?

type T = C :* (Int :@ String >| Boolean >| Int) :* (Int :@ String >| Boolean >| Int)

type T2 =
  Int :@ String
    >| Boolean :? Int
    >| Int :? Boolean

---

-- instance (FromDSL MkStateSpec o, Reflectable o o') => Reflectable MkStateSpec o' where
--   reflectType _ = reflectType (Proxy @o)

-- instance (FromDSL (AddTransition t b) o, Reflectable o StateGraph_) => Reflectable (AddTransition t b) StateGraph_ where
--   reflectType _ = reflectType (Proxy @o)

---

data Wrap a

data Empty

class FromDSL :: forall k1 k2. k1 -> k2 -> Constraint
class FromDSL dsl a | dsl -> a

instance (FromDSL1 a a') => FromDSL (Wrap a) a'

class FromDSL1 :: forall k1 k2. k1 -> k2 -> Constraint
class FromDSL1 dsl a | dsl -> a

instance (FromDSL1 xs (C.MkStateGraph ys)) => FromDSL1 (Empty :* xs) (C.MkStateGraph (ys))

else instance (FromDSL2 x t, FromDSL1 xs (C.MkStateGraph ys)) => FromDSL1 (x :* xs) (C.MkStateGraph (t :> ys))

else instance (FromDSL2 x t) => FromDSL1 x (C.MkStateGraph (t :> Nil'))

class FromDSL2 :: forall k1 k2. k1 -> k2 -> Constraint
class FromDSL2 dsl a | dsl -> a

instance (FromDSL2 x (C.MkTransition s m xs), FromDSL3 y y') => FromDSL2 (x >| y) (C.MkTransition s m (y' :> xs))

instance FromDSL2 (x :@ y) (C.MkTransition x y Nil')

class FromDSL3 :: forall k1 k2. k1 -> k2 -> Constraint
class FromDSL3 dsl a | dsl -> a

instance FromDSL3 (g :? s) (C.MkReturnVia g s)

else instance FromDSL3 s (C.MkReturn s)

