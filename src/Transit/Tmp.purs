module Transit.Tmp where

import Prelude

import Data.Tuple.Nested (type (/\), (/\))

class Build xs z a | xs z -> a where
  build :: (xs -> z) -> a

instance Build Unit z z where
  build f = f unit

instance (Build xs z a) => Build (xs /\ x) z (x -> a) where
  build f = \x -> build (\xs -> f (xs /\ x))

---

-- g :: (Unit) -> Boolean
-- g _ = true

-- f :: (Number /\ (String /\ Unit)) -> Boolean
-- f _ = true

-- x = build f 78.0 ""

--ff = build f