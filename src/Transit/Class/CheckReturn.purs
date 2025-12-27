-- | Type classes for checking and expanding partial return types to full state types.

module Transit.Class.CheckReturn
  ( class CheckReturn
  , checkReturn
  , checkReturnFast
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

checkReturnFast
  :: forall @returns state msg m rowIn rowOut
   . (CheckReturn returns rowIn rowOut)
  => (state -> msg -> m (Variant rowIn))
  -> (state -> msg -> m (Variant rowOut))
checkReturnFast = unsafeCoerce

-- | Checks and expands return types by removing `Ret` and `RetVia` wrappers from variant types.
-- |
-- | The functional dependency `returns -> rowIn rowOut` ensures that given the
-- | return list, both input and output row types are uniquely determined.
-- |
-- | - `returns`: List of return type-level specifications
-- | - `rowIn`: Input row type (with wrappers)
-- | - `rowOut`: Output row type (without wrappers)
class
  CheckReturn (returns :: List' ReturnTL) (rowIn :: Row Type) (rowOut :: Row Type)
  | returns -> rowIn rowOut where
  checkReturn :: Variant rowIn -> Variant rowOut

instance checkReturnNil :: CheckReturn Nil' () ()
  where
  checkReturn = identity

instance checkReturnConsReturn ::
  ( Row.Cons symState payload rowOut' rowOut
  , Row.Cons symState (Ret payload) rowIn' rowIn
  , CheckReturn rest rowIn' rowOut'
  , IsSymbol symState
  , Row.Union rowOut' rowExtra rowOut
  ) =>
  CheckReturn (MkReturnTL symState :> rest) rowIn rowOut
  where
  checkReturn v = out
    where
    out :: Variant rowOut
    out = V.on (Proxy @symState) handleHead handleRest v

    handleHead :: Ret payload -> Variant rowOut
    handleHead (Ret value) = V.inj (Proxy @symState) value

    handleRest :: Variant rowIn' -> Variant rowOut
    handleRest = checkReturn @rest @rowIn' >>> V.expand

instance checkReturnConsReturnVia ::
  ( Row.Cons symState payload rowOut' rowOut
  , Row.Cons symState (RetVia symGuard payload) rowIn' rowIn
  , CheckReturn rest rowIn' rowOut'
  , IsSymbol symState
  , Row.Union rowOut' rowExtra rowOut
  ) =>
  CheckReturn (MkReturnViaTL symGuard symState :> rest) rowIn rowOut
  where
  checkReturn v = out
    where
    out :: Variant rowOut
    out = V.on (Proxy @symState) handleHead handleRest v

    handleHead :: RetVia symGuard payload -> Variant rowOut
    handleHead (RetVia value) = V.inj (Proxy @symState) value

    handleRest :: Variant rowIn' -> Variant rowOut
    handleRest = checkReturn @rest @rowIn' >>> V.expand
