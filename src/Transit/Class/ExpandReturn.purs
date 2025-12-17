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
  ExpandReturn (returns :: List' ReturnTL) full part
  | returns full -> part
  where
  expandReturn :: part -> full

instance expandReturnInst ::
  ( RemoveWrappers returns r r'
  , Row.Union r' rx r''
  ) =>
  ExpandReturn returns (Variant r'') (Variant r) where
  expandReturn part = full
    where
    full :: Variant r''
    full = V.expand cleanedPart

    cleanedPart :: Variant r'
    cleanedPart = removeWrappers @returns @r part

---

class RemoveWrappers (returns :: List' ReturnTL) (rin :: Row Type) (rout :: Row Type) | returns -> rin rout where
  removeWrappers :: Variant rin -> Variant rout

instance removeWrappersNil :: RemoveWrappers Nil' () ()
  where
  removeWrappers = identity

instance removeWrappersConsState ::
  ( Row.Cons symState a rout' rout
  , Row.Cons symState (Ret a) rin' rin
  , RemoveWrappers returns rin' rout'
  , IsSymbol symState
  , Row.Union rout' routx rout
  ) =>
  RemoveWrappers (MkReturnTL symState :> returns) rin rout
  where
  removeWrappers = V.on (Proxy @symState) handleHead handleRest
    where
    handleHead :: Ret a -> Variant rout
    handleHead (Ret x) = V.inj (Proxy @symState) x

    handleRest :: Variant rin' -> Variant rout
    handleRest = removeWrappers @returns @rin' >>> V.expand

instance removeWrappersConsStateVia ::
  ( Row.Cons symState a rout' rout
  , Row.Cons symState (RetVia symGuard a) rin' rin
  , RemoveWrappers returns rin' rout'
  , IsSymbol symState
  , Row.Union rout' routx rout
  ) =>
  RemoveWrappers (MkReturnViaTL symGuard symState :> returns) rin rout
  where
  removeWrappers = V.on (Proxy @symState) handleHead handleRest
    where
    handleHead :: RetVia symGuard a -> Variant rout
    handleHead (RetVia x) = V.inj (Proxy @symState) x

    handleRest :: Variant rin' -> Variant rout
    handleRest = removeWrappers @returns @rin' >>> V.expand
