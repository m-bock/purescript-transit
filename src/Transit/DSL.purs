-- | Domain-Specific Language (DSL) for building type-level transit specifications.
-- |
-- | This module provides a type-level DSL for constructing state machine
-- | specifications in a declarative way. The DSL uses infix operators to create
-- | readable specifications that are automatically converted to transit cores.
-- |
-- | ## Basic Usage
-- |
-- | ```purescript
-- | type MyTransit =
-- |   Transit
-- |     :* ("State1" :@ "Msg1" >| "State2")
-- |     :* ("State2" :@ "Msg2" >| "State1")
-- | ```
-- |
-- | ## Operators
-- |
-- | - `:*` - Add a match/transition to the specification
-- | - `:@` - Combine state and message (e.g., `"State1" :@ "Msg1"`)
-- | - `>|` - Add return state(s) to a match
-- | - `|<` - Create bidirectional edge (e.g., `"State1" |< "Msg" >| "State2"`)
-- | - `:?` - Add guard condition to a return (e.g., `"guard" :? "State2"`)
module Transit.DSL
  ( type (:*)
  , AddMatch
  , type (:?)
  , type (:@)
  , type (>|)
  , type (|<)
  , class ToTransitCore
  , class ToMatch
  , class ToReturn
  , Transit
  , StateWithMsg
  , WithGuard
  , AddOut
  , AddIn
  ) where

import Data.Reflectable (class Reflectable, reflectType)
import Transit.Core (class IsTransitSpec, TransitCore)
import Transit.Core as C
import Type.Data.List (type (:>), Nil')
import Type.Proxy (Proxy(..))

--------------------------------------------------------------------------------
--- DSL Types
--------------------------------------------------------------------------------

-- | Internal type for adding a match to a transit specification.
-- | Used with the `:*` operator.
data AddMatch :: forall k1 k2. k1 -> k2 -> Type
data AddMatch a b

-- | Internal type for combining a state with a message.
-- | Used with the `:@` operator.
data StateWithMsg :: forall k1 k2. k1 -> k2 -> Type
data StateWithMsg a b

-- | Internal type for adding an input (bidirectional edge).
-- | Used with the `|<` operator.
data AddIn :: forall k1 k2. k1 -> k2 -> Type
data AddIn a b

-- | Internal type for adding an output (return state).
-- | Used with the `>|` operator.
data AddOut :: forall k1 k2. k1 -> k2 -> Type
data AddOut a b

-- | Internal type for adding a guard condition to a return.
-- | Used with the `:?` operator.
data WithGuard :: forall k1 k2. k1 -> k2 -> Type
data WithGuard a b

--------------------------------------------------------------------------------
--- Type Operators
--------------------------------------------------------------------------------

-- | Infix operator for adding matches to a transit specification.
-- |
-- | Right-associative, allowing `Transit` to appear first for clean indentation.
-- |
-- | Example: `Transit :* ("State1" :@ "Msg1" >| "State2")`
infixr 0 type AddMatch as :*

-- | Infix operator for combining a state with a message.
-- |
-- | Example: `"State1" :@ "Msg1"`
infixl 5 type StateWithMsg as :@

-- | Infix operator for adding return states to a match.
-- |
-- | Example: `"State1" :@ "Msg1" >| "State2" >| "State3"`
infixl 5 type AddOut as >|

-- | Infix operator for creating bidirectional edges.
-- |
-- | Example: `"State1" |< "Msg" >| "State2"` creates edges in both directions.
infixl 5 type AddIn as |<

-- | Infix operator for adding guard conditions to returns.
-- |
-- | Example: `"guard" :? "State2"`
infixl 9 type WithGuard as :?

--------------------------------------------------------------------------------
--- Reflection Instance
--------------------------------------------------------------------------------

-- | Allows DSL specifications to be reflected to runtime TransitCore values.
instance (IsTransitSpec (AddMatch a b) o, Reflectable o TransitCore) => Reflectable (AddMatch a b) TransitCore where
  reflectType _ = reflectType (Proxy @o)

--------------------------------------------------------------------------------
--- Transit Base Type
--------------------------------------------------------------------------------

-- | Base type for transit specifications (empty specification).
-- |
-- | Start all transit specifications with `Transit`, then add matches using `:*`.
-- |
-- | Example:
-- | ```purescript
-- | type MyTransit =
-- |   Transit
-- |     :* ("State1" :@ "Msg1" >| "State2")
-- |     :* ("State2" :@ "Msg2" >| "State3")
-- | ```
data Transit

-- | Empty transit specification (no matches).
instance IsTransitSpec Transit (C.MkTransitCoreTL Nil')

-- | Transit specification with matches added via `:*`.
instance (ToTransitCore (AddMatch a b) c) => IsTransitSpec (AddMatch a b) c

--------------------------------------------------------------------------------
--- ToTransitCore Type Class
--------------------------------------------------------------------------------

-- | Type class that converts DSL expressions to transit core specifications.
-- |
-- | This class is used internally to transform the DSL syntax into the
-- | underlying transit core type-level representation.
class ToTransitCore :: forall k. k -> C.TransitCoreTL -> Constraint
class ToTransitCore dsl a | dsl -> a

instance ToTransitCore Transit (C.MkTransitCoreTL Nil')

else instance (ToTransitCore xs (C.MkTransitCoreTL ys)) => ToTransitCore (Transit :* xs) (C.MkTransitCoreTL ys)

else instance
  ( ToTransitCore xs (C.MkTransitCoreTL ys)
  ) =>
  ToTransitCore ((s1 |< ms >| s2) :* xs)
    ( C.MkTransitCoreTL
        ( C.MkMatchTL s1 ms (C.MkReturnTL s2 :> Nil')
            :> C.MkMatchTL s2 ms (C.MkReturnTL s1 :> Nil')
            :> ys
        )
    )

else instance
  ToTransitCore (s1 |< ms >| s2)
    ( C.MkTransitCoreTL
        ( C.MkMatchTL s1 ms (C.MkReturnTL s2 :> Nil')
            :> C.MkMatchTL s2 ms (C.MkReturnTL s1 :> Nil')
            :> Nil'
        )
    )

else instance (ToMatch x t, ToTransitCore xs (C.MkTransitCoreTL ys)) => ToTransitCore (x :* xs) (C.MkTransitCoreTL (t :> ys))

else instance (ToMatch x t) => ToTransitCore x (C.MkTransitCoreTL (t :> Nil'))

--------------------------------------------------------------------------------
--- ToMatch Type Class
--------------------------------------------------------------------------------

-- | Type class that converts DSL match expressions to match specifications.
-- |
-- | Handles conversion of state-message combinations and return states
-- | into the underlying match type-level representation.
class ToMatch :: forall k. k -> C.MatchTL -> Constraint
class ToMatch dsl a | dsl -> a

instance (ToMatch x (C.MkMatchTL s m xs), ToReturn y y') => ToMatch (x >| y) (C.MkMatchTL s m (y' :> xs))

instance ToMatch (x :@ y) (C.MkMatchTL x y Nil')

instance ToMatch (x |< y >| z) (C.MkMatchTL x y Nil')

--------------------------------------------------------------------------------
--- ToReturn Type Class
--------------------------------------------------------------------------------

-- | Type class that converts DSL return expressions to return specifications.
-- |
-- | Handles conversion of state names and guard conditions into the
-- | underlying return type-level representation.
class ToReturn :: forall k. k -> C.ReturnTL -> Constraint
class ToReturn dsl a | dsl -> a

instance ToReturn (g :? s) (C.MkReturnViaTL g s)

else instance ToReturn s (C.MkReturnTL s)

