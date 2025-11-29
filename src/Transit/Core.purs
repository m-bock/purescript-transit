module Transit.Core where

import Data.Newtype (class Newtype)
import Type.Data.List (List')

type StateName = Symbol

type MsgName = Symbol

---

foreign import data StateGraph :: Type

foreign import data MkStateGraph :: List' Transition -> StateGraph

---

foreign import data Transition :: Type

foreign import data MkTransition :: StateName -> MsgName -> List' Return -> Transition

---

foreign import data Return :: Type

foreign import data MkReturn :: StateName -> Return

type GuardName = Symbol

foreign import data MkReturnVia :: GuardName -> StateName -> Return

newtype RetVia (symGuard :: Symbol) (a :: Type) = RetVia a

derive instance Newtype (RetVia symGuard a) _