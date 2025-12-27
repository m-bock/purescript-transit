module Transit.Data.MaybeChurch where

import Data.Function (identity)
import Data.Maybe (Maybe(..))

newtype MaybeChurch a = MaybeChurch (forall z. z -> (a -> z) -> z)

toMaybe :: forall a. MaybeChurch a -> Maybe a
toMaybe (MaybeChurch f) = f Nothing Just

nothingChurch :: forall a. MaybeChurch a
nothingChurch = MaybeChurch \no _ -> no

justChurch :: forall a. a -> MaybeChurch a
justChurch a = MaybeChurch \_ yes -> yes a

runMaybeChurch :: forall a z. z -> (a -> z) -> MaybeChurch a -> z
runMaybeChurch no yes (MaybeChurch f) = f no yes

fromMaybeChurch :: forall a. a -> MaybeChurch a -> a
fromMaybeChurch a (MaybeChurch f) = f a identity