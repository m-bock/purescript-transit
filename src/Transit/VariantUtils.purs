module Transit.VariantUtils where

import Data.Symbol (class IsSymbol)
import Data.Variant (Variant)
import Data.Variant as V
import Prim.Row as Row
import Type.Equality (class TypeEquals)
import Type.Prelude (Proxy(..))

class Inj (sym :: Symbol) a where
  inj :: a

instance (Row.Cons sym a r1 r2, IsSymbol sym) => Inj sym (a -> Variant r2) where
  inj = V.inj (Proxy :: _ sym)

else instance (Row.Cons sym {} r1 r2, IsSymbol sym) => Inj sym (Variant r2) where
  inj = V.inj (Proxy :: _ sym) {}

-- class Inj (sym :: Symbol) v a | v -> sym a where
--   inj :: a

-- instance (Row.Cons sym a r1 r2, IsSymbol sym) => Inj sym (Variant r2) (a -> Variant r2) where
--   inj = V.inj (Proxy :: _ sym)

-- instance (Row.Cons sym {} r1 r2, IsSymbol sym) => Inj sym (Variant r2) (Variant r2) where
--   inj = V.inj (Proxy :: _ sym) {}

-- type R = (a :: Int, b :: String)

-- type T = Variant (a :: Int, b :: String)

-- --x :: T

-- x = myInj @"a" 3

-- myInj :: forall @sym a. Inj sym T a => a
-- myInj = inj @sym @T

-- xx = myInj @"a" 3