module Transit.Util where

import Prelude

import Type.Data.List (Cons')

type Snoc' xs x = Cons' x xs

infixl 5 type Snoc' as :<

type Flip :: forall k1 k2 k3. (k1 -> k2 -> k3) -> k2 -> k1 -> k3
type Flip f a b = f b a
