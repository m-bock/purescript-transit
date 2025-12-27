-- | Type classes for expanding partial return types to full state types.

module Transit.Class.ExpandReturn
  ( class RemoveWrappers
  , removeWrappers
  , removeWrappersFast
  ) where

import Prelude

import Data.Symbol (class IsSymbol)
import Data.Variant (Variant)
import Data.Variant as V
import Prim.Row as Row
import Transit.Core (MkReturnTL, MkReturnViaTL, ReturnTL, Ret(..), RetVia(..))
import Type.Data.List (type (:>), List', Nil')
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

---

removeWrappersFast
  :: forall @returns state msg m rowIn rowOut
   . (RemoveWrappers returns rowIn rowOut)
  => (state -> msg -> m (Variant rowIn))
  -> (state -> msg -> m (Variant rowOut))
removeWrappersFast = unsafeCoerce

-- | Removes `Ret` and `RetVia` wrappers from variant types.
-- |
-- | The functional dependency `returns -> rowIn rowOut` ensures that given the
-- | return list, both input and output row types are uniquely determined.
-- |
-- | - `returns`: List of return type-level specifications
-- | - `rowIn`: Input row type (with wrappers)
-- | - `rowOut`: Output row type (without wrappers)
class
  RemoveWrappers (returns :: List' ReturnTL) (rowIn :: Row Type) (rowOut :: Row Type)
  | returns -> rowIn rowOut where
  removeWrappers :: Variant rowIn -> Variant rowOut

instance removeWrappersNil :: RemoveWrappers Nil' () ()
  where
  removeWrappers = identity

instance removeWrappersConsReturn ::
  ( Row.Cons symState payload rowOut' rowOut
  , Row.Cons symState (Ret payload) rowIn' rowIn
  , RemoveWrappers rest rowIn' rowOut'
  , IsSymbol symState
  , Row.Union rowOut' rowExtra rowOut
  ) =>
  RemoveWrappers (MkReturnTL symState :> rest) rowIn rowOut
  where
  removeWrappers v = out
    where
    out :: Variant rowOut
    out = V.on (Proxy @symState) handleHead handleRest v

    handleHead :: Ret payload -> Variant rowOut
    handleHead (Ret value) = V.inj (Proxy @symState) value

    handleRest :: Variant rowIn' -> Variant rowOut
    handleRest = removeWrappers @rest @rowIn' >>> V.expand

instance removeWrappersConsReturnVia ::
  ( Row.Cons symState payload rowOut' rowOut
  , Row.Cons symState (RetVia symGuard payload) rowIn' rowIn
  , RemoveWrappers rest rowIn' rowOut'
  , IsSymbol symState
  , Row.Union rowOut' rowExtra rowOut
  ) =>
  RemoveWrappers (MkReturnViaTL symGuard symState :> rest) rowIn rowOut
  where
  removeWrappers v = out
    where
    out :: Variant rowOut
    out = V.on (Proxy @symState) handleHead handleRest v

    handleHead :: RetVia symGuard payload -> Variant rowOut
    handleHead (RetVia value) = V.inj (Proxy @symState) value

    handleRest :: Variant rowIn' -> Variant rowOut
    handleRest = removeWrappers @rest @rowIn' >>> V.expand
