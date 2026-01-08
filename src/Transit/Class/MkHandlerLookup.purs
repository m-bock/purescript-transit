-- | Type class for building handler lookups from transit specifications.

module Transit.Class.MkHandlerLookup
  ( class MkHandlerLookupBuilder
  , mkHandlerLookupBuilder
  , class MkHandlerLookup
  , mkHandlerLookup
  ) where

import Prelude

import Data.Symbol (class IsSymbol)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Variant (Variant)
import Prim.Row as Row
import Transit.Class.CheckReturn (class CheckReturn, checkReturnFast)
import Transit.Core (MatchImpl(..), MatchTL, MkMatchTL)
import Transit.HandlerLookup (HandlerLookup, HandlerLookupBuilder, addHandler, build, initBuilder)
import Type.Data.List (type (:>), List', Nil')

class
  MkHandlerLookup
    (m :: Type -> Type)
    (spec :: List' MatchTL)
    matches
    (rowState :: Row Type)
    (rowMsg :: Row Type)
  | spec rowState rowMsg m -> matches where
  mkHandlerLookup :: matches -> HandlerLookup m rowState rowMsg

instance mkHandlerLookupInst ::
  ( MkHandlerLookupBuilder m spec matches rowState rowMsg
  , Applicative m
  ) =>
  MkHandlerLookup m spec matches rowState rowMsg where
  mkHandlerLookup matches = build (mkHandlerLookupBuilder @m @spec matches)

class
  MkHandlerLookupBuilder
    (m :: Type -> Type)
    (spec :: List' MatchTL)
    matches
    (rowState :: Row Type)
    (rowMsg :: Row Type)
  | spec rowState rowMsg m -> matches where
  mkHandlerLookupBuilder :: matches -> HandlerLookupBuilder m rowState rowMsg

instance mkHandlerLookupBuilderNil ::
  MkHandlerLookupBuilder m Nil' Unit rowState rowMsg where
  mkHandlerLookupBuilder _ = out
    where
    out :: HandlerLookupBuilder m rowState rowMsg
    out = initBuilder @rowState @rowMsg

instance mkHandlerLookupBuilderCons ::
  ( IsSymbol symStateIn
  , IsSymbol symMsg
  , Functor m

  , CheckReturn returns rowStateOut rowStateOutClean
  , MkHandlerLookupBuilder m restSpec restMatches rowState rowMsg

  , Row.Cons symStateIn stateIn _rowState rowState
  , Row.Cons symMsg msgIn _rowMsg rowMsg
  , Row.Union rowStateOutClean _rowStateOutOthers rowState
  ) =>
  MkHandlerLookupBuilder
    m
    (MkMatchTL symStateIn symMsg returns :> restSpec)
    (MatchImpl symStateIn symMsg stateIn msgIn m (Variant rowStateOut) /\ restMatches)
    rowState
    rowMsg
  where
  mkHandlerLookupBuilder (MatchImpl handlerRaw /\ restMatches) = out
    where
    out :: HandlerLookupBuilder m rowState rowMsg
    out = addHandler @symStateIn @symMsg handler builder

    builder :: HandlerLookupBuilder m rowState rowMsg
    builder = mkHandlerLookupBuilder @m @restSpec restMatches

    handler :: stateIn -> msgIn -> m (Variant rowStateOutClean)
    handler = checkReturnFast @returns handlerRaw

