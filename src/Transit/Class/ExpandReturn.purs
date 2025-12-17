module Transit.Class.ExpandReturn
  ( class ExpandReturn
  , expandReturn
  , class RemoveWrappers
  , removeWrappers
  ) where

import Prelude

import Data.Symbol (class IsSymbol)
import Data.Variant (Variant)
import Data.Variant as V
import Prim.Row as Row
import Transit.Core (MkReturnTL, MkReturnViaTL, ReturnTL, Ret(..), RetVia(..))
import Type.Data.List (type (:>), List', Nil')
import Type.Proxy (Proxy(..))

class
  ExpandReturn (syms :: List' ReturnTL) ty a
  | syms ty -> a
  where
  expandReturn :: a -> ty

instance expandReturnInst ::
  ( RemoveWrappers syms r r'
  , Row.Union r' rx r''
  ) =>
  ExpandReturn syms (Variant r'') (Variant r) where
  expandReturn v = y
    where
    y :: Variant r''
    y = V.expand x

    x :: Variant r'
    x = removeWrappers @syms @r v

---

class RemoveWrappers (syms :: List' ReturnTL) (rin :: Row Type) (rout :: Row Type) | syms -> rin rout where
  removeWrappers :: Variant rin -> Variant rout

instance removeWrappersNil :: RemoveWrappers Nil' () ()
  where
  removeWrappers = identity

instance removeWrappersConsState ::
  ( Row.Cons symState a rout' rout
  , Row.Cons symState (Ret a) rin' rin
  , RemoveWrappers syms rin' rout'
  , IsSymbol symState
  , Row.Union rout' routx rout
  ) =>
  RemoveWrappers (MkReturnTL symState :> syms) rin rout
  where
  removeWrappers v = V.on (Proxy @symState) (\(Ret x) -> V.inj (Proxy @symState) x) (removeWrappers @syms @rin' >>> V.expand) v

instance removeWrappersConsStateVia ::
  ( Row.Cons symState a rout' rout
  , Row.Cons symState (RetVia symGuard a) rin' rin
  , RemoveWrappers syms rin' rout'
  , IsSymbol symState
  , Row.Union rout' routx rout
  ) =>
  RemoveWrappers (MkReturnViaTL symGuard symState :> syms) rin rout
  where
  removeWrappers = V.on (Proxy @symState) (\(RetVia x) -> V.inj (Proxy @symState) x) (removeWrappers @syms @rin' >>> V.expand)

