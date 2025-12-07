module Transit.Util
  ( Generically(..)
  ) where

import Prelude

import Data.Newtype (class Newtype)

newtype Generically a = Generically a

derive instance Newtype (Generically a) _

