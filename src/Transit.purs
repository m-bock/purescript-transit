module Transit
  ( module Export
  , mkUpdateGeneric
  , mkUpdateGenericM
  , match
  , return
  , return_
  , returnVia
  , returnVia_
  , class Return
  , return'
  , class ReturnVia
  , returnVia'
  ) where

import Prelude

import Data.Identity (Identity(..))
import Data.Newtype (un)
import Data.Symbol (class IsSymbol)
import Data.Variant (Variant)
import Data.Variant as V
import Prim.Row as Row
import Transit.Core (Match(..), ReturnState(..), ReturnStateVia(..))
import Transit.DSL (class FromDSL)
import Transit.DSL (class FromDSL, class FromDSL1, class FromDSL2, class FromDSL3, type (:*), type (:?), type (:@), type (>|), AddOut, D, Empty, J, StateWithMsg, Wrap) as Export
import Transit.MkUpdate (class MkUpdate, mkUpdate)
import Transit.Tmp (class Build, build)
import Transit.Util (Generically(..))
import Type.Prelude (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

-- mkUpdateGeneric :: forall @dsl spec impl msg state. (FromDSL dsl spec) => (MkUpdate spec Identity impl (Generically msg) (Generically state)) => impl -> state -> msg -> state
-- mkUpdateGeneric impl msg state = un Generically $ un Identity $ mkUpdate @spec impl (Generically msg) (Generically state)

-- mkUpdateGenericM :: forall @spec m impl msg state. Applicative m => (MkUpdate spec m impl (Generically msg) (Generically state)) => impl -> state -> msg -> m state
-- mkUpdateGenericM impl msg state = map (un Generically) $ mkUpdate @spec impl (Generically msg) (Generically state)

mkUpdateGenericM
  :: forall @dsl spec m msg state xs a
   . (Functor m)
  => (FromDSL dsl spec)
  => (Build xs (state -> msg -> m state) a)
  => (MkUpdate spec m xs (Generically msg) (Generically state))
  => a
mkUpdateGenericM = build @xs f
  where
  f :: xs -> state -> msg -> m state
  f impl state msg = map (un Generically) $ mkUpdate @spec @m @xs impl (Generically state) (Generically msg)

mkUpdateGeneric
  :: forall @dsl spec msg state xs a
   . (FromDSL dsl spec)
  => (Build xs (state -> msg -> state) a)
  => (MkUpdate spec Identity xs (Generically msg) (Generically state))
  => a
mkUpdateGeneric = build @xs f
  where
  f :: xs -> state -> msg -> state
  f impl state msg = un Identity $ map (un Generically) $ mkUpdate @spec @Identity @xs impl (Generically state) (Generically msg)

-- class Mk dsl a where
--   mkIt :: a

-- instance (FromDSL dsl spec, Build xs (state -> msg -> m state) a, MkUpdate spec m xs msg state) => Mk dsl a where
--   mkIt = build @xs (mkUpdate @spec @m @xs @msg @state)

-- mkGeneric :: forall @spec @m @msg @state @xs a. (Build xs (state -> msg -> m state) a) => (MkUpdate spec m xs msg state) => a
-- mkGeneric = build @xs (mkUpdate @spec @m @xs @msg @state)

-- ---

match :: forall @symState @symMsg msgIn stateIn stateOut. (msgIn -> stateIn -> stateOut) -> Match symState symMsg msgIn stateIn stateOut
match f = Match f

-- ---

return :: forall (@sym ∷ Symbol) (a ∷ Type) (r1 ∷ Row Type) (r2 ∷ Row Type). Row.Cons sym (ReturnState a) r1 r2 ⇒ IsSymbol sym ⇒ a → Variant r2
return v = V.inj (Proxy :: _ sym) (ReturnState v)

return_ :: forall (@sym ∷ Symbol) (r1 ∷ Row Type) (r2 ∷ Row Type). Row.Cons sym (ReturnState Unit) r1 r2 ⇒ IsSymbol sym ⇒ Variant r2
return_ = V.inj (Proxy :: _ sym) (ReturnState unit)

class Return (sym :: Symbol) a where
  return' :: a

instance (Row.Cons sym (ReturnState a) r1 r2, IsSymbol sym) => Return sym (a -> Variant r2) where
  return' v = V.inj (Proxy :: _ sym) (ReturnState v)

instance (Row.Cons sym (ReturnState Unit) r1 r2, IsSymbol sym) => Return sym (Variant r2) where
  return' = V.inj (Proxy :: _ sym) (ReturnState unit)

returnVia :: forall (@symGuard :: Symbol) (@sym ∷ Symbol) (a ∷ Type) (r1 ∷ Row Type) (r2 ∷ Row Type). Row.Cons sym (ReturnStateVia symGuard a) r1 r2 ⇒ IsSymbol sym ⇒ a → Variant r2
returnVia v = V.inj (Proxy :: _ sym) (ReturnStateVia @symGuard v)

returnVia_ :: forall (@symGuard :: Symbol) (@sym ∷ Symbol) (r1 ∷ Row Type) (r2 ∷ Row Type). Row.Cons sym (ReturnStateVia symGuard Unit) r1 r2 ⇒ IsSymbol sym ⇒ Variant r2
returnVia_ = V.inj (Proxy :: _ sym) (ReturnStateVia @symGuard unit)

class ReturnVia (symGuard :: Symbol) (sym :: Symbol) a where
  returnVia' :: a

instance (Row.Cons sym (ReturnStateVia symGuard a) r1 r2, IsSymbol sym) => ReturnVia symGuard sym (a -> Variant r2) where
  returnVia' v = V.inj (Proxy :: _ sym) (ReturnStateVia @symGuard v)

instance (Row.Cons sym (ReturnStateVia symGuard Unit) r1 r2, IsSymbol sym) => ReturnVia symGuard sym (Variant r2) where
  returnVia' = V.inj (Proxy :: _ sym) (ReturnStateVia @symGuard unit)