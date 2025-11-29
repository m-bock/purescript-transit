module Transit.Facade where

import Prelude

import Safe.Coerce (coerce)
import Transit.Core (Return, StateGraph, StateName)
import Transit.Core as C
import Transit.Util (Generically(..))

type GuardName = Symbol

foreign import data MkReturnVia :: GuardName -> StateName -> Return

newtype Ret (symGuard :: Symbol) (a :: Type) = Ret a

mkUpdateG :: forall @spec impl msg state. (MkUpdate spec impl (Generically msg) (Generically state)) => impl -> msg -> state -> state
mkUpdateG impl msg state = coerce $ mkUpdate @spec impl (Generically msg) (Generically state)

---

class MkUpdate :: StateGraph -> Type -> Type -> Type -> Constraint
class MkUpdate spec impl msg state | spec msg state -> impl where
  mkUpdate :: impl -> msg -> state -> state

instance
  ( C.MkUpdate spec' impl' msg state
  , Transform spec spec' impl impl'
  ) =>
  MkUpdate spec impl msg state where
  mkUpdate impl msg state = C.mkUpdate @spec' (transformImpl @spec impl) msg state

class Transform (spec :: StateGraph) (spec' :: StateGraph) impl impl' | impl spec -> impl' spec' where
  transformImpl :: impl -> impl'

instance Transform spec spec impl impl where
  transformImpl impl = impl