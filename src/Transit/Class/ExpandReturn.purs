-- | Type classes for expanding partial return types to full state types.

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
import Unsafe.Coerce (unsafeCoerce)

-- | Expands a partial variant (containing `Ret` and `RetVia` wrappers) into
-- | a full variant by removing the wrappers and expanding to a larger row type.
-- |
-- | The functional dependency `returns full -> part` ensures that given the
-- | return list and full variant type, the partial variant type is uniquely determined.
-- |
-- | - `returns`: List of return type-level specifications
-- | - `full`: The full variant type (target)
-- | - `part`: The partial variant type (source, with wrappers)
class
  ExpandReturn (returns :: List' ReturnTL) full part
  | returns full -> part
  where
  expandReturn :: part -> full

instance expandReturnInstance ::
  ( RemoveWrappers returns rowIn rowOut
  , Row.Union rowOut rowExtra rowFull
  ) =>
  ExpandReturn returns (Variant rowFull) (Variant rowIn) where
  expandReturn part = full
    where
    full :: Variant rowFull
    full = V.expand cleanedPart

    cleanedPart :: Variant rowOut
    cleanedPart = removeWrappers @returns @rowIn part

---

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
  removeWrappers v = unsafeCoerce v
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
  removeWrappers v = unsafeCoerce v
    where
    out :: Variant rowOut
    out = V.on (Proxy @symState) handleHead handleRest v

    handleHead :: RetVia symGuard payload -> Variant rowOut
    handleHead (RetVia value) = V.inj (Proxy @symState) value

    handleRest :: Variant rowIn' -> Variant rowOut
    handleRest = removeWrappers @rest @rowIn' >>> V.expand
