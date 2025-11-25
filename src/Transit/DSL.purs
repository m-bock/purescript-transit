module Transit.DSL where

foreign import data SG :: Type

foreign import data MkSG :: SG

foreign import data AddTransition :: Transition' -> SG -> SG

--

foreign import data TransitionBuilderStep1 :: Type

foreign import data TransitionBuilderInit :: Symbol -> Symbol -> TransitionBuilderStep1

--

foreign import data Transition' :: Type

foreign import data TransitionBuilderAddRet :: Symbol -> TransitionBuilderStep1 -> Transition'

foreign import data TransitionBuilderAddExtraRet :: Symbol -> Transition' -> Transition'

--

type AddTransitionFlipped a b = AddTransition b a
type TransitionBuilderAddExtraRetFlipped a b = TransitionBuilderAddExtraRet b a
type TransitionBuilderAddRetFlipped a b = TransitionBuilderAddRet b a
type TransitionBuilderInitFlipped a b = TransitionBuilderInit b a

infixl 5 type AddTransitionFlipped as :*

infixl 5 type TransitionBuilderAddExtraRetFlipped as :|

infixl 5 type TransitionBuilderAddRetFlipped as :->

infixl 5 type TransitionBuilderInitFlipped as :@
