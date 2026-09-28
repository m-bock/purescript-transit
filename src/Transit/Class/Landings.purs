-- | Where a message can land, read off the specification.
-- |
-- | A caller that emits a message and then wants to know the state
-- | usually knows which states that message can produce - the table
-- | says so - and today that knowledge lives in a comment, or in an
-- | exception thrown when the state is not what was expected. These
-- | classes compute it instead.
module Transit.Class.Landings
  ( class Landings
  , class LandingsOfMatches
  , class LandingsOfReturns
  ) where

import Prim.Row as Row
import Transit.Core (class IsTransitSpec, MatchTL, MkMatchTL, MkReturnTL, MkReturnViaTL, MkTransitCoreTL, ReturnTL)
import Type.Data.List (type (:>), List', Nil')

-- | The states a message can produce, as a row of the same labels the
-- | state variant uses.
-- |
-- | This is the only class a caller names: the walk behind it and the
-- | translation from the DSL are its business, not theirs.
class
  Landings :: forall k. k -> Symbol -> Row Type -> Row Type -> Constraint
class
  Landings spec msg (rowState :: Row Type) (landings :: Row Type)
  | spec msg rowState -> landings

instance
  ( IsTransitSpec spec (MkTransitCoreTL matches)
  , LandingsOfMatches matches msg rowState landings
  ) =>
  Landings spec msg rowState landings

-- | Every match for this message, and nothing about the others.
class
  LandingsOfMatches :: List' MatchTL -> Symbol -> Row Type -> Row Type -> Constraint
class
  LandingsOfMatches (matches :: List' MatchTL) msg (rowState :: Row Type) (landings :: Row Type)
  | matches msg rowState -> landings

instance LandingsOfMatches Nil' msg rowState ()

else instance
  ( LandingsOfMatches rest msg rowState fromRest
  , LandingsOfReturns returns rowState fromRest landings
  ) =>
  LandingsOfMatches (MkMatchTL from msg returns :> rest) msg rowState landings

else instance
  ( LandingsOfMatches rest msg rowState landings
  ) =>
  LandingsOfMatches (MkMatchTL from other returns :> rest) msg rowState landings

-- | One match may have several returns - a guard is how - so each
-- | adds its target to what the caller must be ready for.
-- |
-- | The target's label comes from the specification and its payload
-- | from the state row, so the two cannot disagree. `Row.Nub` is what
-- | lets several arrows land in the same state without saying it
-- | twice, which is the common case: every arrow labelled `Asked` in a
-- | page's table ends in the same place.
class
  LandingsOfReturns :: List' ReturnTL -> Row Type -> Row Type -> Row Type -> Constraint
class
  LandingsOfReturns (returns :: List' ReturnTL) (rowState :: Row Type) (acc :: Row Type) (landings :: Row Type)
  | returns rowState acc -> landings

instance LandingsOfReturns Nil' rowState acc acc

else instance
  ( LandingsOfReturns rest rowState acc soFar
  , Row.Cons to payload restState rowState
  , Row.Cons to payload soFar withTarget
  , Row.Nub withTarget landings
  ) =>
  LandingsOfReturns (MkReturnTL to :> rest) rowState acc landings

else instance
  ( LandingsOfReturns rest rowState acc soFar
  , Row.Cons to payload restState rowState
  , Row.Cons to payload soFar withTarget
  , Row.Nub withTarget landings
  ) =>
  LandingsOfReturns (MkReturnViaTL guard to :> rest) rowState acc landings
