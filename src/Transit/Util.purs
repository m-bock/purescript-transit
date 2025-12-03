module Transit.Util where

import Prelude

import Data.Newtype (class Newtype)
import Type.Data.List (Cons', List')

type Snoc' xs x = Cons' x xs

infixl 5 type Snoc' as :<

-- type Flip :: forall k1 k2 k3. (k1 -> k2 -> k3) -> k2 -> k1 -> k3
-- type Flip f a b = f b a

newtype Generically a = Generically a

derive instance Newtype (Generically a) _

type Id :: forall k. k -> k
type Id a = a

data Tup :: forall k1 k2. k1 -> k2 -> Type
data Tup a b

infixl 5 type Tup as :*