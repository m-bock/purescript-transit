module Transit.Class.MatchBySym where

import Prelude

import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), NoArguments(..), Sum(..), from)
import Transit.Util (Generically(..))
import Type.Proxy (Proxy(..))

class
  MatchBySym (sym :: Symbol) ty a
  | sym ty -> a
  where
  matchBySym :: forall z. (a -> z) -> (Unit -> z) -> ty -> z

instance (Generic ty rep, MatchBySym sym rep a) => MatchBySym sym (Generically ty) a
  where
  matchBySym onMatch onDefault (Generically x) = matchBySym @sym onMatch onDefault $ from x

instance MatchBySym sym (Constructor sym (Argument a)) a where
  matchBySym onMatch _ (Constructor (Argument x)) = onMatch x

else instance MatchBySym sym (Constructor sym NoArguments) Unit where
  matchBySym onMatch _ (Constructor NoArguments) = onMatch unit

else instance MatchBySym sym (Constructor sym2 b) a where
  matchBySym _ onDefault _ = onDefault unit

instance (MatchBySym sym t1 a, MatchBySym sym t2 a) => MatchBySym sym (Sum t1 t2) a where
  matchBySym onMatch onDefault (Inl x) = matchBySym @sym onMatch onDefault x
  matchBySym onMatch onDefault (Inr x) = matchBySym @sym onMatch onDefault x

matchBySym2 :: forall @sym1 @sym2 a1 b1 a2 b2 z. MatchBySym sym1 a1 b1 => MatchBySym sym2 a2 b2 => (b1 -> b2 -> z) -> (Unit -> z) -> a1 -> a2 -> z
matchBySym2 f z x1 x2 =
  matchBySym @sym1 (\y1 -> matchBySym @sym2 (\y2 -> f y1 y2) z x2) z x1

checkMatchBySym :: forall sym a b. MatchBySym sym a b => Proxy sym -> Proxy a -> Proxy b -> Unit
checkMatchBySym _ _ _ = unit

data Test = Test1 Int | Test2 String

derive instance Generic Test _

test1 :: Unit
test1 = checkMatchBySym (Proxy :: _ "Test1") (Proxy :: _ (Generically Test)) (Proxy :: _ Int)

test2 :: Unit
test2 = checkMatchBySym (Proxy :: _ "Test2") (Proxy :: _ (Generically Test)) (Proxy :: _ String)