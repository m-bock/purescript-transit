module Transit
  ( module Export
  , mkUpdateGeneric
  , mkUpdateGenericM
  , match
  , return
  , returnVia
  , class Return
  , class ReturnVia
  ) where

import Prelude

import Data.Identity (Identity(..))
import Data.Newtype (un)
import Data.Symbol (class IsSymbol)
import Data.Variant (Variant)
import Data.Variant as V
import Prim.Row as Row
import Transit.Core (class IsTransitSpec, MatchImpl(..), ReturnState(..), ReturnStateVia(..))
import Transit.CurryN (class CurryN, curryN)
import Transit.DSL as Export
import Transit.Class.MkUpdate (class MkUpdate, mkUpdate)
import Transit.Util (Generically(..))
import Type.Prelude (Proxy(..))

mkUpdateGenericM
  :: forall @dsl spec m msg state xs a
   . (Functor m)
  => (IsTransitSpec dsl spec)
  => (CurryN xs (state -> msg -> m state) a)
  => (MkUpdate spec m xs (Generically msg) (Generically state))
  => a
mkUpdateGenericM = curryN @xs f
  where
  f :: xs -> state -> msg -> m state
  f impl state msg = map (un Generically) $ mkUpdate @spec @m @xs impl (Generically state) (Generically msg)

mkUpdateGeneric
  :: forall @dsl spec msg state xs a
   . (IsTransitSpec dsl spec)
  => (CurryN xs (state -> msg -> state) a)
  => (MkUpdate spec Identity xs (Generically msg) (Generically state))
  => a
mkUpdateGeneric = curryN @xs f
  where
  f :: xs -> state -> msg -> state
  f impl state msg = un Identity $ map (un Generically) $ mkUpdate @spec @Identity @xs impl (Generically state) (Generically msg)

match :: forall @symState @symMsg msgIn stateIn stateOut. (msgIn -> stateIn -> stateOut) -> MatchImpl symState symMsg msgIn stateIn stateOut
match f = MatchImpl f

class Return (sym :: Symbol) a where
  return :: a

instance (Row.Cons sym (ReturnState a) r1 r2, IsSymbol sym) => Return sym (a -> Variant r2) where
  return v = V.inj (Proxy :: _ sym) (ReturnState v)

instance (Row.Cons sym (ReturnState Unit) r1 r2, IsSymbol sym) => Return sym (Variant r2) where
  return = V.inj (Proxy :: _ sym) (ReturnState unit)

class ReturnVia (symGuard :: Symbol) (sym :: Symbol) a where
  returnVia :: a

instance (Row.Cons sym (ReturnStateVia symGuard a) r1 r2, IsSymbol sym) => ReturnVia symGuard sym (a -> Variant r2) where
  returnVia v = V.inj (Proxy :: _ sym) (ReturnStateVia @symGuard v)

instance (Row.Cons sym (ReturnStateVia symGuard Unit) r1 r2, IsSymbol sym) => ReturnVia symGuard sym (Variant r2) where
  returnVia = V.inj (Proxy :: _ sym) (ReturnStateVia @symGuard unit)