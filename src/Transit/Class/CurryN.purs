-- | A type class for converting functions that take nested tuple arguments
-- | into curried functions.
-- |
-- | Example:
-- | ```purescript
-- | f :: Int /\ String /\ Unit -> String
-- | f (x /\ s /\ _) = s <> show x
-- |
-- | curried :: Int -> String -> String
-- | curried = curryN f
-- | ```

module Transit.Class.CurryN
  ( class CurryN
  , curryN
  ) where

import Prelude

import Data.Tuple.Nested (type (/\), (/\))

-- | Converts a function from nested tuple arguments to a curried function.
-- |
-- | - `args`: The nested tuple type of arguments (must end with `Unit`)
-- | - `result`: The return type of the function
-- | - `curried`: The curried function type
class CurryN args result curried | args result -> curried, args curried -> result where
  curryN :: (args -> result) -> curried

-- | Base case: when the tuple is `Unit`, the function has no arguments
-- | and we simply apply it to `unit`.
instance curryNUnit :: CurryN Unit result result where
  curryN f = f unit

-- | Recursive case: for `arg /\ args`, we curry the first argument `arg`
-- | and recursively curry the remaining arguments `args`.
instance curryNCons :: (CurryN args result curried) => CurryN (arg /\ args) result (arg -> curried) where
  curryN f = \arg -> curryN (\args -> f (arg /\ args))
