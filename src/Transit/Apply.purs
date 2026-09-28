-- | Applying a message, and being handed only the states it can
-- | produce.
module Transit.Apply
  ( class Applying
  , applying
  , mkApply
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Variant (Variant, contract)
import Data.Variant.Internal (class Contractable)
import Transit.Class.Landings (class Landings)
import Type.Proxy (Proxy(..))

-- | Where one message can land, and how to get there. Everything the
-- | walk over the specification needs is in the instance, so a caller
-- | writes this constraint and no other.
class
  Applying :: forall k. k -> Symbol -> Row Type -> Row Type -> Constraint
class
  Applying spec msg (rowState :: Row Type) (landings :: Row Type)
  | spec msg rowState -> landings where
  applying
    :: forall m rowMsg
     . Functor m
    => Proxy spec
    -> Proxy msg
    -> (Variant rowMsg -> m (Variant rowState))
    -> Variant rowMsg
    -> m (Either (Variant rowState) (Variant landings))

instance
  ( Landings spec msg rowState landings
  , Contractable rowState landings
  ) =>
  Applying spec msg rowState landings where
  applying _ _ apply msg =
    apply msg <#> \produced -> case contract produced of
      Just landed -> Right landed
      Nothing -> Left produced

-- | Turns a machine's `apply` - a message in, the state it produced
-- | out - into one that answers with the states this message can
-- | produce, and with the whole state when it produced none of them.
-- |
-- | ```purescript
-- | landed <- mkApply @Spec @"Asked" api.applyMsg msg
-- | ```
mkApply
  :: forall @spec @msg m rowMsg rowState landings
   . Functor m
  => Applying spec msg rowState landings
  => (Variant rowMsg -> m (Variant rowState))
  -> Variant rowMsg
  -> m (Either (Variant rowState) (Variant landings))
mkApply apply msg = applying (Proxy :: Proxy spec) (Proxy :: Proxy msg) apply msg

-- ## Context
--
-- **`Either`, not a label.** An earlier version answered with one
-- `Variant` carrying the landings plus a `stuck` field, so a caller
-- had a single `match`. A machine with a state called `stuck` would
-- then have produced a row with that label twice - which rows allow,
-- and which no `match` can take apart. `Left` is a name the caller's
-- state row cannot collide with.
--
-- **`mkApply` is not the class method**, because a method cannot carry
-- visible type arguments: they belong to the class head, where `@` may
-- not be written. `mkApply` is the method with `spec` and `msg` made
-- visible, and it is what a caller uses. `applying` is exported only
-- because a class's members go where the class goes; it takes `spec`
-- and `msg` as proxies because nothing in its value type mentions
-- them, and without them a use site has nothing to solve from.
