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

class
  MatchBySym (sym :: Symbol) ty a
  | sym ty -> a
  where
  matchBySym :: forall z. (a -> z) -> (Unit -> z) -> ty -> z

instance matchBySymInst :: (Row.Cons sym a r1 r2, IsSymbol sym) => MatchBySym sym (Variant r2) a where
  matchBySym onMatch onDefault = V.on (Proxy @sym) onMatch (\_ -> onDefault unit)

matchBySym2 :: forall @sym1 @sym2 a1 b1 a2 b2 z. MatchBySym sym1 a1 b1 => MatchBySym sym2 a2 b2 => (b1 -> b2 -> z) -> (Unit -> z) -> a1 -> a2 -> z
matchBySym2 f z x1 x2 =
  matchBySym @sym1 (\y1 -> matchBySym @sym2 (\y2 -> f y1 y2) z x2) z x1
