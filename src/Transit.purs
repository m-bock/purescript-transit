module Transit
  ( module Export
  , mkUpdateEitherM
  , mkUpdateEither
  , mkUpdateM
  , mkUpdate
  , match
  , matchM
  , return
  , returnVia
  , class Return
  , class ReturnVia
  ) where

import Prelude

import Data.Bifunctor (bimap)
import Data.Either (Either, fromRight)
import Data.Identity (Identity(..))
import Data.Symbol (class IsSymbol)
import Data.Tuple.Nested ((/\))
import Data.Variant (Variant)
import Data.Variant as V
import Prim.Coerce (class Coercible)
import Prim.Row as Row
import Safe.Coerce as Safe
import Transit.Class.CurryN (class CurryN, curryN)
import Transit.Class.MkUpdate (class MkUpdate, TransitError)
import Transit.Class.MkUpdate as MU
import Transit.Core (class IsTransitSpec, MatchImpl(..), ReturnState(..), ReturnStateVia(..))
import Transit.DSL (class ToMatch, class ToReturn, class ToTransitCore, type (:*), type (:?), type (:@), type (>|), AddMatch, AddOut, Empty, StateWithMsg, Transit, WithGuard) as Export
import Transit.Util (Generically(..))
import Type.Prelude (Proxy(..))

-- mkUpdateEitherM
-- mkUpdateEither
-- mkUpdateM
-- mkUpdate

-- mkUpdateGenericEitherM
-- mkUpdateGenericEither
-- mkUpdateGenericM
-- mkUpdateGeneric

mkUpdateEitherM
  :: forall @spec tcore msg state args m a
   . (IsTransitSpec spec tcore)
  => (CurryN args (state -> msg -> m (Either (TransitError state msg) state)) a)
  => (MkUpdate tcore m args msg state)
  => a
mkUpdateEitherM = curryN @args f
  where
  f :: args -> state -> msg -> m (Either (TransitError state msg) state)
  f impl state msg =
    MU.mkUpdate @tcore impl state msg

mkUpdateEither
  :: forall @spec tcore msg state args a
   . (IsTransitSpec spec tcore)
  => (CurryN args (state -> msg -> Either (TransitError state msg) state) a)
  => (MkUpdate tcore Identity args msg state)
  => a
mkUpdateEither = curryN @args f
  where
  f :: args -> state -> msg -> Either (TransitError state msg) state
  f impl state msg =
    safeUnwrap @Identity $
      (MU.mkUpdate @tcore impl state msg)

mkUpdateM
  :: forall @spec tcore msg state args m a
   . (IsTransitSpec spec tcore)
  => (CurryN args (state -> msg -> m state) a)
  => (MkUpdate tcore m args msg state)
  => Functor m
  => a
mkUpdateM = curryN @args f
  where
  f :: args -> state -> msg -> m state
  f impl state msg =
    map (fromRight state)
      (MU.mkUpdate @tcore impl state msg)

mkUpdate
  :: forall @spec tcore msg state args a
   . (IsTransitSpec spec tcore)
  => (CurryN args (state -> msg -> state) a)
  => (MkUpdate tcore Identity args msg state)
  => a
mkUpdate = curryN @args f
  where
  f :: args -> state -> msg -> state
  f impl state msg =
    fromRight state $ safeUnwrap @Identity $ (MU.mkUpdate @tcore impl state msg)

safeUnwrap :: forall @f a. Coercible (f a) a => f a -> a
safeUnwrap = Safe.coerce

safeWrap :: forall @f a. Coercible a (f a) => a -> f a
safeWrap = Safe.coerce

match :: forall @symState @symMsg msgIn stateIn stateOut. (msgIn -> stateIn -> stateOut) -> MatchImpl symState symMsg Identity msgIn stateIn stateOut
match f = MatchImpl (\msg state -> pure $ f msg state)

matchM :: forall @symState @symMsg m msgIn stateIn stateOut. (msgIn -> stateIn -> m stateOut) -> MatchImpl symState symMsg m msgIn stateIn stateOut
matchM f = MatchImpl (\msg state -> f msg state)

class Return (sym :: Symbol) a where
  return :: a

instance (Row.Cons sym (ReturnState a) r1 r2, IsSymbol sym) => Return sym (a -> Variant r2) where
  return v = V.inj (Proxy :: _ sym) (ReturnState v)

instance (Row.Cons sym (ReturnState {}) r1 r2, IsSymbol sym) => Return sym (Variant r2) where
  return = V.inj (Proxy :: _ sym) (ReturnState {})

class ReturnVia (symGuard :: Symbol) (sym :: Symbol) a where
  returnVia :: a

instance (Row.Cons sym (ReturnStateVia symGuard a) r1 r2, IsSymbol sym) => ReturnVia symGuard sym (a -> Variant r2) where
  returnVia v = V.inj (Proxy :: _ sym) (ReturnStateVia @symGuard v)

instance (Row.Cons sym (ReturnStateVia symGuard {}) r1 r2, IsSymbol sym) => ReturnVia symGuard sym (Variant r2) where
  returnVia = V.inj (Proxy :: _ sym) (ReturnStateVia @symGuard {})

---

mapTransitError :: forall msg1 msg2 state1 state2. (msg1 -> msg2) -> (state1 -> state2) -> TransitError state1 msg1 -> TransitError state2 msg2
mapTransitError mapMsg mapState (state /\ msg) = (mapState state /\ mapMsg msg)

mapTransitResult :: forall msg1 msg2 state1 state2. (msg1 -> msg2) -> (state1 -> state2) -> Either (TransitError state1 msg1) state1 -> Either (TransitError state2 msg2) state2
mapTransitResult mapMsg mapState = bimap (mapTransitError mapMsg mapState) mapState