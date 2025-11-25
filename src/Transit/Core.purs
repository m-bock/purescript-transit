module Transit.Core where

import Type.Data.List (List')

foreign import data StateGraph :: Type

foreign import data MkStateGraph :: List' Transition -> StateGraph

---

foreign import data StateName :: Type

foreign import data MkStateName :: Symbol -> StateName

---

foreign import data MsgName :: Type

foreign import data MkMsgName :: Symbol -> MsgName

---

foreign import data Transition :: Type

foreign import data MkTransition :: StateName -> MsgName -> List' StateName -> Transition