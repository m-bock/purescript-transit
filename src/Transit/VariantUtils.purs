-- | Convenience utilities for creating Variant values.
-- |
-- | This module provides a type-safe way to create Variant values using
-- | type application, eliminating the need for Proxy values and allowing
-- | empty record arguments to be omitted.
-- |
-- | ## Usage
-- |
-- | For variants with empty payloads (empty record):
-- | ```purescript
-- | v @"DoorOpen" :: State
-- | ```
-- |
-- | For variants with non-empty payloads:
-- | ```purescript
-- | v @"DoorLocked" { activePin: "1234" } :: State
-- | ```
module Transit.VariantUtils
  ( v
  , class Inj
  ) where

import Data.Symbol (class IsSymbol)
import Data.Variant (Variant)
import Data.Variant as V
import Prim.Row as Row
import Type.Prelude (Proxy(..))

--------------------------------------------------------------------------------
--- Variant Injection Helper
--------------------------------------------------------------------------------

-- | Type class for injecting values into Variants using type application.
-- |
-- | The `v` function provides a convenient way to create Variant values
-- | without needing Proxy values. It automatically handles both empty
-- | and non-empty payloads.
class Inj (sym :: Symbol) a where
  -- | Creates a Variant value by injecting a value into the specified variant label.
  -- |
  -- | For variants with non-empty payloads, the payload must be provided:
  -- | ```purescript
  -- | v @"Label" payload :: Variant (label :: PayloadType | r)
  -- | ```
  -- |
  -- | For variants with empty payloads (empty record), the payload
  -- | can be omitted:
  -- | ```purescript
  -- | v @"Label" :: Variant (label :: {} | r)
  -- | ```
  v :: a

-- | Instance for variants with non-empty payloads.
-- | Requires the payload to be provided as an argument.
instance (Row.Cons sym a r1 r2, IsSymbol sym) => Inj sym (a -> Variant r2) where
  v = V.inj (Proxy :: _ sym)

-- | Instance for variants with empty payloads (empty record).
-- | Automatically provides the empty record argument.
else instance (Row.Cons sym {} r1 r2, IsSymbol sym) => Inj sym (Variant r2) where
  v = V.inj (Proxy :: _ sym) {}
