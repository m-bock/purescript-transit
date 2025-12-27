-- | Type class for building handler lookups from transit specifications.

module Transit.Class.MkHandlerLookup
  ( class MkHandlerLookup
  , mkHandlerLookup
  ) where

import Prelude

import Data.Symbol (class IsSymbol)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Variant (Variant)
import Prim.Row as Row
import Transit.Class.CheckReturn (class CheckReturn, checkReturnFast)
import Transit.Core (MatchImpl(..), MatchTL, MkMatchTL)
import Transit.HandlerLookup (HandlerLookupBuilder, addHandler, initBuilder)
import Type.Data.List (type (:>), List', Nil')

class
  MkHandlerLookup
    (m :: Type -> Type)
    (spec :: List' MatchTL)
    matches
    (rowState :: Row Type)
    (rowMsg :: Row Type)
  | spec rowState rowMsg m -> matches where
  mkHandlerLookup :: matches -> HandlerLookupBuilder m rowState rowMsg

instance mkHandlerLookupNil :: MkHandlerLookup m (Nil') Unit rowState rowMsg where
  mkHandlerLookup _ = initBuilder @rowState @rowMsg

instance mkHandlerLookupCons ::
  ( IsSymbol symStateIn
  , IsSymbol symMsg
  , CheckReturn returns rowStateOut rowStateOut'
  , Row.Cons symStateIn stateIn _x1 rowState
  , Row.Cons symMsg msgIn _x2 rowMsg
  , Row.Union rowStateOut' _x3 rowState
  , Functor m
  , MkHandlerLookup m (rest1) rest2 rowState rowMsg
  ) =>
  MkHandlerLookup m
    (MkMatchTL symStateIn symMsg returns :> rest1)
    (MatchImpl symStateIn symMsg stateIn msgIn m (Variant rowStateOut) /\ rest2)
    rowState
    rowMsg
  where
  mkHandlerLookup (MatchImpl fn /\ rest) = out
    where
    out = addHandler @symStateIn @symMsg fn' builder
    builder = mkHandlerLookup @m @(rest1) rest

    fn' :: stateIn -> msgIn -> m (Variant rowStateOut')
    fn' = checkReturnFast @returns fn

