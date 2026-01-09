module Transit.Class.MkAutoHandlers where

import Prelude

import Data.Symbol (class IsSymbol)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Variant (Variant)
import Data.Variant as V
import Prim.Row as Row
import Transit.Core (MatchImpl(..))
import Type.Prelude (Proxy(..))
import Prim.RowList as RL

class MkAutoHandlers args where
  mkAutoHandlers :: args

instance mkAutoHandlersNil :: MkAutoHandlers Unit where
  mkAutoHandlers = unit

instance mkAutoHandlersCons ::
  ( MkAutoHandlers rest
  , Row.Cons symStateOut stateInOut () rowStateOut
  , RL.RowToList rowStateOut (RL.Cons symStateOut stateInOut RL.Nil)
  , IsSymbol symStateOut
  , Applicative m
  ) =>
  MkAutoHandlers (MatchImpl symStateIn symMsg stateInOut msgIn m (Variant rowStateOut) /\ rest) where
  mkAutoHandlers = head /\ tail
    where
    head :: MatchImpl symStateIn symMsg stateInOut msgIn m (Variant rowStateOut)
    head = MatchImpl (\st _ -> pure (V.inj (Proxy @symStateOut) st))

    tail :: rest
    tail = mkAutoHandlers

