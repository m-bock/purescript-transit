module Transit.VariantUtils where

import Data.Symbol (class IsSymbol)
import Data.Variant (Variant)
import Data.Variant as V
import Prim.Row as Row
import Type.Equality (class TypeEquals)
import Type.Prelude (Proxy(..))

class Inj (sym :: Symbol) a where
  v :: a

instance (Row.Cons sym a r1 r2, IsSymbol sym) => Inj sym (a -> Variant r2) where
  v = V.inj (Proxy :: _ sym)

else instance (Row.Cons sym {} r1 r2, IsSymbol sym) => Inj sym (Variant r2) where
  v = V.inj (Proxy :: _ sym) {}
