module Transit.Data.MaybeChurch where

newtype MaybeChurch a = MaybeChurch (forall z. z -> (a -> z) -> z)

toMaybe :: MaybeChurch a -> Maybe a
toMaybe (MaybeChurch f) = f Nothing Just
