-- | Type class for matching types by symbol and extracting associated payload types.

module Transit.Class.MatchBySym
  ( class MatchBySym
  , matchBySym
  , matchBySym2
  ) where

import Prelude

import Data.Symbol (class IsSymbol)
import Data.Variant (Variant)
import Data.Variant as V
import Prim.Row as Row
import Type.Prelude (Proxy(..))

-- | Matches a type by symbol and extracts the associated payload type.
-- |
-- | The functional dependency `sym container -> payload` ensures that given the
-- | symbol and container type, the payload type is uniquely determined.
-- |
-- | - `sym`: The symbol key to match on
-- | - `container`: The container type (e.g., a Variant)
-- | - `payload`: The payload type associated with the symbol
class
  MatchBySym (sym :: Symbol) container payload
  | sym container -> payload
  where
  matchBySym :: forall result. (payload -> result) -> (Unit -> result) -> container -> result

-- | Instance for matching Variant types by symbol.
instance matchBySymInstance :: (Row.Cons sym payload row1 row2, IsSymbol sym) => MatchBySym sym (Variant row2) payload where
  matchBySym onMatch onDefault = V.on (Proxy @sym) onMatch (\_ -> onDefault unit)

-- | Matches two containers by their respective symbols and combines the results.
-- |
-- | This function matches both containers simultaneously, extracting their payloads
-- | and applying a function to combine them. If either container doesn't match,
-- | the default handler is called.
matchBySym2
  :: forall @sym1 @sym2 container1 payload1 container2 payload2 result
   . MatchBySym sym1 container1 payload1
  => MatchBySym sym2 container2 payload2
  => (payload1 -> payload2 -> result)
  -> (Unit -> result)
  -> container1
  -> container2
  -> result
matchBySym2 onMatch onDefault variant1 variant2 =
  matchBySym @sym1
    (\payload1 -> matchBySym @sym2 (\payload2 -> onMatch payload1 payload2) onDefault variant2)
    onDefault
    variant1
