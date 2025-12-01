module Transit.DSL where

import Data.Reflectable (class Reflectable, reflectType)
import Data.Unit (Unit, unit)
import Transit.Core (MkReturn, MkReturnVia, StateGraph, StateGraph_(..))
import Transit.Core as C
import Transit.Util (type (:<))
import Type.Data.List (Cons', Nil')
import Type.Proxy (Proxy(..))

foreign import data StateSpec :: Type

foreign import data MkStateSpec :: StateSpec

foreign import data AddTransition :: TransitionBuilderFinal -> StateSpec -> StateSpec

--

foreign import data TransitionBuilderStep1 :: Type

foreign import data TransitionBuilderInit :: Symbol -> Symbol -> TransitionBuilderStep1

--

foreign import data TransitionBuilderFinal :: Type

foreign import data TransitionBuilderAddRet :: Symbol -> TransitionBuilderStep1 -> TransitionBuilderFinal

foreign import data TransitionBuilderAddRetVia :: Symbol -> Symbol -> TransitionBuilderStep1 -> TransitionBuilderFinal

foreign import data TransitionBuilderAddExtraRet :: Symbol -> TransitionBuilderFinal -> TransitionBuilderFinal

--

type AddTransitionFlipped a b = AddTransition b a
type TransitionBuilderAddExtraRetFlipped a b = TransitionBuilderAddExtraRet b a
type TransitionBuilderAddRetFlipped a b = TransitionBuilderAddRet b a

infixl 5 type AddTransitionFlipped as :*

infixl 5 type TransitionBuilderAddExtraRetFlipped as :|

infixl 5 type TransitionBuilderAddRetFlipped as :>

infixl 5 type TransitionBuilderInit as :@

---

-- instance (FromDSL MkStateSpec o, Reflectable o o') => Reflectable MkStateSpec o' where
--   reflectType _ = reflectType (Proxy @o)

instance (FromDSL (AddTransition t b) o, Reflectable o StateGraph_) => Reflectable (AddTransition t b) StateGraph_ where
  reflectType _ = reflectType (Proxy @o)

---

class FromDSL :: forall k1 k2. k1 -> k2 -> Constraint
class FromDSL dsl a | dsl -> a

---

instance FromDSL MkStateSpec (C.MkStateGraph Nil')

instance
  ( FromDSL rest (C.MkStateGraph xs)
  , FromDSL a a'
  ) =>
  FromDSL (AddTransition a rest) (C.MkStateGraph (xs :< a'))

-- ---

instance
  FromDSL
    (TransitionBuilderAddRet symStateOut (TransitionBuilderInit symStateIn symMsg))
    (C.MkTransition symStateIn symMsg (Cons' (MkReturn symStateOut) Nil'))

instance
  FromDSL
    (TransitionBuilderAddRetVia symGuard symStateOut (TransitionBuilderInit symStateIn symMsg))
    (C.MkTransition symStateIn symMsg (Cons' (MkReturnVia symGuard symStateOut) Nil'))

instance
  ( FromDSL rest (C.MkTransition stateIn msg symsStateOut)
  ) =>
  FromDSL
    (TransitionBuilderAddExtraRet symStateOut rest)
    (C.MkTransition stateIn msg (Cons' (MkReturn symStateOut) symsStateOut))
