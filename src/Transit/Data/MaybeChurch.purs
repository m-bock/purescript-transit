module Transit.Data.MaybeChurch where

import Prelude

import Control.Alt (class Alt)
import Control.Alternative (class Alternative)
import Control.Plus (class Plus)
import Data.Maybe (Maybe(..))

newtype MaybeChurch a = MaybeChurch (forall z. z -> (a -> z) -> z)

instance Functor MaybeChurch where
  map f (MaybeChurch g) = MaybeChurch \no yes -> g no (f >>> yes)

instance Apply MaybeChurch where
  apply mf mx = match nothing (\f -> match nothing (\x -> just (f x)) mx) mf

instance Applicative MaybeChurch where
  pure = just

instance Alt MaybeChurch where
  alt ma mb = match mb (const ma) ma

instance Plus MaybeChurch where
  empty = nothing

instance Alternative MaybeChurch

toMaybe :: forall a. MaybeChurch a -> Maybe a
toMaybe (MaybeChurch f) = f Nothing Just

nothing :: forall a. MaybeChurch a
nothing = MaybeChurch \no _ -> no

just :: forall a. a -> MaybeChurch a
just a = MaybeChurch \_ yes -> yes a

match :: forall a z. z -> (a -> z) -> MaybeChurch a -> z
match no yes (MaybeChurch f) = f no yes

fromMaybeChurch :: forall a. a -> MaybeChurch a -> a
fromMaybeChurch a (MaybeChurch f) = f a identity