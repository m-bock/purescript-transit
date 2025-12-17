module Transit.Class.GetSubset
  ( class GetSubset
  , getSubset
  , class RemoveWrappers
  , removeWrappers
  ) where

import Data.Function (identity)
import Data.Unit (Unit, unit)
import Data.Variant (Variant)
import Data.Variant as V
import Prim.Row as Row
import Transit.Core (MkReturnTL, MkReturnViaTL, ReturnTL, Via)
import Type.Data.List (type (:>), Cons', List', Nil')
import Unsafe.Coerce (unsafeCoerce)

class
  GetSubset (syms :: List' ReturnTL) ty a
  | syms ty -> a
  where
  getSubset :: a -> ty

instance getSubsetInst ::
  ( RemoveWrappers syms r r'
  , Row.Union r' rx r''
  ) =>
  GetSubset syms (Variant r'') (Variant r) where
  getSubset v = y
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
  , Row.Cons symState a rin' rin
  , RemoveWrappers syms rin' rout'
  ) =>
  RemoveWrappers (MkReturnTL symState :> syms) rin rout
  where
  removeWrappers = unsafeCoerce -- "because no change of runtime representation, just unwrapping newtypes"

instance removeWrappersConsStateVia ::
  ( Row.Cons symState a rout' rout
  , Row.Cons symState (Via symGuard a) rin' rin
  , RemoveWrappers syms rin' rout'
  ) =>
  RemoveWrappers (MkReturnViaTL symGuard symState :> syms) rin rout
  where
  removeWrappers = unsafeCoerce -- "because no change of runtime representation, just unwrapping newtypes"

checkRemoveWrappers :: forall @syms @rin @rout. (RemoveWrappers syms rin rout) => Unit
checkRemoveWrappers = unit

test1 :: Unit
test1 = checkRemoveWrappers
  @Nil'
  @()
  @()

test2 :: Unit
test2 = checkRemoveWrappers
  @(Cons' (MkReturnTL "Foo") Nil')
  @("Foo" :: Int)
  @("Foo" :: Int)

test3 :: Unit
test3 = checkRemoveWrappers
  @(Cons' (MkReturnTL "Foo") (Cons' (MkReturnViaTL "Transition" "Bar") Nil'))
  @("Foo" :: Int, "Bar" :: Via "Transition" String)
  @("Foo" :: Int, "Bar" :: String)

