module Transit (module Export) where

import Prelude
import Transit.DSL as Export

-- mkUpdateG :: forall @spec impl msg state. (MkUpdate spec impl (Generically msg) (Generically state)) => impl -> msg -> state -> state
-- mkUpdateG impl msg state = coerce $ mkUpdate @spec impl (Generically msg) (Generically state)

-- ---

-- match :: forall @symState @symMsg msgIn stateIn stateOut. (msgIn -> stateIn -> stateOut) -> Match symState symMsg msgIn stateIn stateOut
-- match f = Match f

-- ---

-- return :: forall (@sym ∷ Symbol) (a ∷ Type) (r1 ∷ Row Type) (r2 ∷ Row Type). Row.Cons sym a r1 r2 ⇒ IsSymbol sym ⇒ a → Variant r2
-- return v = V.inj (Proxy :: _ sym) v

