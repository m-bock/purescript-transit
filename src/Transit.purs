module Transit
  ( class Return
  , class ReturnVia
  , match
  , match'
  , matchM
  , mkUpdate
  , mkUpdateEither
  , mkUpdateEitherM
  , mkUpdateM
  , module Export
  , return
  , returnVia
  ) where

import Prelude

import Data.Either (Either, fromRight)
import Data.Identity (Identity(..))
import Data.Symbol (class IsSymbol)
import Data.Variant (Variant)
import Data.Variant as V
import Prim.Coerce (class Coercible)
import Prim.Row as Row
import Safe.Coerce as Safe
import Transit.Class.CurryN (class CurryN, curryN)
import Transit.Class.MkUpdate (class MkUpdate, TransitError)
import Transit.Class.MkUpdate as MU
import Transit.Core (class IsTransitSpec, MatchImpl(..), ReturnState(..), Via(..))
import Transit.DSL (class ToMatch, class ToReturn, class ToTransitCore, type (:*), type (:?), type (:@), type (>|), AddMatch, AddOut, Empty, StateWithMsg, WithGuard) as Export
import Type.Prelude (Proxy(..))

mkUpdateEitherM
  :: forall @spec tcore msg state args m a
   . (IsTransitSpec spec tcore)
  => (CurryN args (state -> msg -> m (Either TransitError state)) a)
  => (MkUpdate tcore m args msg state)
  => a
mkUpdateEitherM = curryN @args f
  where
  f :: args -> state -> msg -> m (Either TransitError state)
  f impl state msg =
    MU.mkUpdate @tcore impl state msg

mkUpdateEither
  :: forall @spec tcore msg state args a
   . (IsTransitSpec spec tcore)
  => (CurryN args (state -> msg -> Either TransitError state) a)
  => (MkUpdate tcore Identity args msg state)
  => a
mkUpdateEither = curryN @args f
  where
  f :: args -> state -> msg -> Either TransitError state
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

match :: forall @symState @symMsg msgIn stateIn stateOut. (msgIn -> stateIn -> stateOut) -> MatchImpl symState symMsg Identity msgIn stateIn stateOut
match f = MatchImpl (\msg state -> pure $ f msg state)

match' :: forall @symState @symMsg msgIn stateIn stateOut. ({ msg :: msgIn, state :: stateIn } -> stateOut) -> MatchImpl symState symMsg Identity stateIn msgIn stateOut
match' f = MatchImpl (\state msg -> pure $ f { msg, state })

matchM :: forall @symState @symMsg m msgIn stateIn stateOut. (msgIn -> stateIn -> m stateOut) -> MatchImpl symState symMsg m msgIn stateIn stateOut
matchM f = MatchImpl (\msg state -> f msg state)

class Return (sym :: Symbol) a where
  return :: a

instance (Row.Cons sym a r1 r2, IsSymbol sym) => Return sym (a -> Variant r2) where
  return v = V.inj (Proxy :: _ sym) v

instance (Row.Cons sym {} r1 r2, IsSymbol sym) => Return sym (Variant r2) where
  return = V.inj (Proxy :: _ sym) {}

class ReturnVia (symGuard :: Symbol) (sym :: Symbol) a where
  returnVia :: a

instance (Row.Cons sym (Via symGuard a) r1 r2, IsSymbol sym) => ReturnVia symGuard sym (a -> Variant r2) where
  returnVia v = V.inj (Proxy :: _ sym) (Via @symGuard v)

instance (Row.Cons sym (Via symGuard {}) r1 r2, IsSymbol sym) => ReturnVia symGuard sym (Variant r2) where
  returnVia = V.inj (Proxy :: _ sym) (Via @symGuard {})
