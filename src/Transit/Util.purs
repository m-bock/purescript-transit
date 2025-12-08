module Transit.Util
  ( Generically(..)
  ) where

import Prelude

import Data.Newtype (class Newtype)

newtype Generically a = Generically a

derive instance Newtype (Generically a) _
derive instance Eq a => Eq (Generically a)

derive newtype instance Show a => Show (Generically a)