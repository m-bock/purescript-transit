module Transit.Tmp where

import Prelude

import Data.Tuple.Nested (type (/\), (/\))
import Type.Data.List (class Concat, type (:>), Nil')

class Build xs z a | xs z -> a where
  build :: (xs -> z) -> a

instance Build Unit z z where
  build f = f unit

instance (Build xs z a) => Build (x /\ xs) z (x -> a) where
  build f = \x -> build (\xs -> f (x /\ xs))

---

-- g :: (Unit) -> Boolean
-- g _ = true

-- f :: (Number /\ (String /\ Unit)) -> Boolean
-- f _ = true

-- x = build f 78.0 ""

--ff = build f

class Revert xs ys | xs -> ys

instance Revert Nil' Nil'
instance (Revert xs ys, Concat ys (x :> Nil') zs) => Revert (x :> xs) zs

checkRevert :: forall @xs @ys. (Revert xs ys) => Unit
checkRevert = unit

test1 :: Unit
test1 = checkRevert @Nil' @Nil'

type T1 = Int :> Boolean :> String :> Nil'
type T2 = String :> Boolean :> Int :> Nil'

test2 :: Unit
test2 = checkRevert @T1 @T2

---

data D1 a b

data D2 a b

infixl 5 type D1 as :**

infixr 6 type D2 as :++

type D = Int :** Int
  :** String
    :++ Int
    :++ Int
