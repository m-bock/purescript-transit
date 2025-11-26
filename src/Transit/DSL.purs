module Transit.DSL where

import Data.Unit (Unit, unit)
import Transit.Core as C
import Transit.Util (type (:<))
import Type.Data.List (Nil')
import Type.Proxy (Proxy(..))

foreign import data StateGraphDSL :: Type

foreign import data MkStateGraphDSL :: StateGraphDSL

foreign import data AddTransition :: TransitionBuilderFinal -> StateGraphDSL -> StateGraphDSL

--

foreign import data TransitionBuilderStep1 :: Type

foreign import data TransitionBuilderInit :: Symbol -> Symbol -> TransitionBuilderStep1

--

foreign import data TransitionBuilderFinal :: Type

foreign import data TransitionBuilderAddRet :: Symbol -> TransitionBuilderStep1 -> TransitionBuilderFinal

foreign import data TransitionBuilderAddExtraRet :: Symbol -> TransitionBuilderFinal -> TransitionBuilderFinal

--

type AddTransitionFlipped a b = AddTransition b a
type TransitionBuilderAddExtraRetFlipped a b = TransitionBuilderAddExtraRet b a
type TransitionBuilderAddRetFlipped a b = TransitionBuilderAddRet b a

infixl 5 type AddTransitionFlipped as :*

infixl 5 type TransitionBuilderAddExtraRetFlipped as :|

infixl 5 type TransitionBuilderAddRetFlipped as :->

infixl 5 type TransitionBuilderInit as :@

---

class FromDSL :: forall k1 k2. k1 -> k2 -> Constraint
class FromDSL dsl a | dsl -> a

---

instance FromDSL MkStateGraphDSL (C.MkStateGraph Nil')

instance
  ( FromDSL rest (C.MkStateGraph xs)
  , FromDSL a a'
  ) =>
  FromDSL (AddTransition a rest) (C.MkStateGraph (xs :< a'))

---

instance
  FromDSL
    (TransitionBuilderAddRet symStateOut (TransitionBuilderInit symStateIn symMsg))
    (C.MkTransition (C.MkStateName symStateIn) (C.MkMsgName symMsg) (C.MkStateNameList (Nil' :< symStateOut)))

instance
  ( FromDSL rest (C.MkTransition stateIn msg (C.MkStateNameList symsStateOut))
  ) =>
  FromDSL
    (TransitionBuilderAddExtraRet symStateOut rest)
    (C.MkTransition stateIn msg (C.MkStateNameList (symsStateOut :< symStateOut)))

---

checkFromDSL :: forall a b. (FromDSL a b) => Proxy a -> Proxy b -> Unit
checkFromDSL _ _ = unit

test1 :: Unit
test1 = checkFromDSL
  ( Proxy
      :: Proxy
           ( MkStateGraphDSL
               :* ("State1" :@ "Msg1" :-> "State2")
               :* ("State2" :@ "Msg2" :-> "State3" :| "State1")
               :* ("State3" :@ "Msg3" :-> "State1" :| "State2" :| "State4")
           )
  )
  ( Proxy
      :: Proxy
           ( C.MkStateGraph
               ( Nil'
                   :< (C.MkTransition (C.MkStateName "State1") (C.MkMsgName "Msg1") (C.MkStateNameList (Nil' :< "State2")))
                   :< (C.MkTransition (C.MkStateName "State2") (C.MkMsgName "Msg2") (C.MkStateNameList (Nil' :< "State3" :< "State1")))
                   :< (C.MkTransition (C.MkStateName "State3") (C.MkMsgName "Msg3") (C.MkStateNameList (Nil' :< "State1" :< "State2" :< "State4")))
               ))
           )
  