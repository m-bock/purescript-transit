module Transit.Class.CurryN
  ( class CurryN
  , curryN
  ) where

import Prelude

import Data.Tuple.Nested (type (/\), (/\))

class CurryN xs z a | xs z -> a where
  curryN :: (xs -> z) -> a

instance curryNUnit :: CurryN Unit z z where
  curryN f = f unit

instance curryNCons :: (CurryN xs z a) => CurryN (x /\ xs) z (x -> a) where
  curryN f = \x -> curryN (\xs -> f (x /\ xs))
