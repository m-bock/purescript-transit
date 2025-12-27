-- @inline export mkUpdateInst arity=2

module Transit.Class.MkUpdate where

import Prelude

import Control.Alternative (class Alternative)
import Data.Function.Uncurried (runFn4)
import Data.Variant (Variant)
import Transit.Class.MkHandlerLookup (class MkHandlerLookup, mkHandlerLookup)
import Transit.Core (MkTransitCoreTL, TransitCoreTL)
import Transit.HandlerLookup (build, runI, runImpl)

class
  MkUpdate (spec :: TransitCoreTL) (m :: Type -> Type) (may :: Type -> Type) matches msg state
  | spec msg state m -> matches
  where
  mkUpdateCore :: matches -> state -> msg -> m (may state)

instance mkUpdateInst ::
  ( MkHandlerLookup m spec matches rowState rowMsg
  , Applicative m
  , Alternative may
  ) =>
  MkUpdate (MkTransitCoreTL spec) m may matches (Variant rowMsg) (Variant rowState) where
  mkUpdateCore matches =
    let
      handerLookupBuilder = mkHandlerLookup @m @spec matches
      handlerLookup = build handerLookupBuilder
    in
      \state msg -> runFn4 runImpl runI handlerLookup state msg