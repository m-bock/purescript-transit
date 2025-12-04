module Transit
  ( module Export
  , mkUpdateGeneric
  , mkUpdateGenericM
  , match
  , return
  , return_
  , returnVia
  , returnVia_
  ) where

import Prelude

import Data.Identity (Identity(..))
import Data.Newtype (un)
import Data.Symbol (class IsSymbol)
import Data.Variant (Variant)
import Data.Variant as V
import Prim.Row as Row
import Safe.Coerce (coerce)
import Transit.Core (Match(..), Return, ReturnState(..), ReturnStateVia(..), Return_(..))
import Transit.DSL (class FromDSL)
import Transit.DSL as Export
import Transit.MkUpdate (class MkUpdate, mkUpdate)
import Transit.Util (Generically(..))
import Type.Prelude (Proxy(..))

mkUpdateGeneric :: forall @dsl spec impl msg state. (FromDSL dsl spec) => (MkUpdate spec Identity impl (Generically msg) (Generically state)) => impl -> state -> msg -> state
mkUpdateGeneric impl msg state = un Generically $ un Identity $ mkUpdate @spec impl (Generically msg) (Generically state)

-- mkUpdateGeneric' :: forall @dsl spec impl msg state. (FromDSL dsl spec) => (MkUpdate spec Identity impl (Generically msg) (Generically state)) => impl -> msg -> state -> state
-- mkUpdateGeneric' impl msg state = un Generically $ un Identity $ mkUpdate @spec impl (Generically msg) (Generically state)

mkUpdateGenericM :: forall @spec m impl msg state. Applicative m => (MkUpdate spec m impl (Generically msg) (Generically state)) => impl -> state -> msg -> m state
mkUpdateGenericM impl msg state = map (un Generically) $ mkUpdate @spec impl (Generically msg) (Generically state)

-- ---

match :: forall @symState @symMsg msgIn stateIn stateOut. (msgIn -> stateIn -> stateOut) -> Match symState symMsg msgIn stateIn stateOut
match f = Match f

-- ---

return :: forall (@sym ∷ Symbol) (a ∷ Type) (r1 ∷ Row Type) (r2 ∷ Row Type). Row.Cons sym (ReturnState a) r1 r2 ⇒ IsSymbol sym ⇒ a → Variant r2
return v = V.inj (Proxy :: _ sym) (ReturnState v)

return_ :: forall (@sym ∷ Symbol) (r1 ∷ Row Type) (r2 ∷ Row Type). Row.Cons sym (ReturnState Unit) r1 r2 ⇒ IsSymbol sym ⇒ Variant r2
return_ = V.inj (Proxy :: _ sym) (ReturnState unit)

returnVia :: forall (@symGuard :: Symbol) (@sym ∷ Symbol) (a ∷ Type) (r1 ∷ Row Type) (r2 ∷ Row Type). Row.Cons sym (ReturnStateVia symGuard a) r1 r2 ⇒ IsSymbol sym ⇒ a → Variant r2
returnVia v = V.inj (Proxy :: _ sym) (ReturnStateVia @symGuard v)

returnVia_ :: forall (@symGuard :: Symbol) (@sym ∷ Symbol) (r1 ∷ Row Type) (r2 ∷ Row Type). Row.Cons sym (ReturnStateVia symGuard Unit) r1 r2 ⇒ IsSymbol sym ⇒ Variant r2
returnVia_ = V.inj (Proxy :: _ sym) (ReturnStateVia @symGuard unit)