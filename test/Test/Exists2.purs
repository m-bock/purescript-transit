module Test.Exists2 where

import Unsafe.Coerce (unsafeCoerce)

foreign import data Exists2 :: (Type -> Type -> Type) -> Type

type role Exists2 representational

mkExists2 :: forall f a b. f a b -> Exists2 f
mkExists2 = unsafeCoerce

runExists2 :: forall f r. (forall a b. f a b -> r) -> Exists2 f -> r
runExists2 = unsafeCoerce