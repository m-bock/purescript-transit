module Test.Example where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Data.Unit (unit)
import Data.Variant (Variant)
import Data.Variant as V
import Prim.Row (class Cons)
import Transit.Core (Match(..), MkStateGraph, MkTransition, StateGraph, mkUpdate)
import Transit.DSL (type (:*), type (:->), type (:@), type (:|), AddTransition, MkStateGraphDSL, StateGraphDSL, TransitionBuilderAddExtraRet, TransitionBuilderAddRet, TransitionBuilderInit)
import Transit.Util (type (:<), Generically)
import Type.Data.List (Nil')
import Type.Function (type (#))
import Type.Proxy (Proxy(..))

data Msg = Msg1 { foo :: Int } | Msg2 { bar :: String }

derive instance Generic Msg _

data State = State1 { foo :: Int } | State2 { bar :: String } | State3 { baz :: Boolean }

derive instance Generic State _

type MyStateGraph :: StateGraph
type MyStateGraph = MkStateGraph
  ( Nil'
      :< (MkTransition "State1" "Msg1" (Nil' :< "State2"))
      :< (MkTransition "State2" "Msg2" (Nil' :< "State3" :< "State1"))
  )

type MyStateGraphDSLRep :: StateGraphDSL
type MyStateGraphDSLRep = MkStateGraphDSL
  # AddTransition (TransitionBuilderInit "State1" "Msg1" # TransitionBuilderAddRet "State2")
  # AddTransition (TransitionBuilderInit "State2" "Msg2" # TransitionBuilderAddRet "State3" # TransitionBuilderAddExtraRet "State1")

-- type MyStateGraphDSL :: StateGraphDSL
-- type MyStateGraphDSL = MkStateGraphDSL
--   :* ("State1" :@ "Msg1" :-> Out "State2")
--   :* ("State2" :@ "Msg2" :-> Out "State3" :| Out "State1")

-- type MyStateGraphDSL2 :: StateGraphDSL
-- type MyStateGraphDSL2 = MkStateGraphDSL
--   :* ("State1" :@ "Msg1" :-> "State2")
--   :* ("State2" :@ "Msg2" :-> ("Guard1" : "State3", "Guard2" : "State1"))

-- type MyStateGraphDSL3 :: StateGraphDSL
-- type MyStateGraphDSL3 = MkStateGraphDSL
--   :* ("State1" :@ "Msg1" :-> "State2")
--   :* ("State2" :@ "Msg2" :-> ("Guard1" :? "State3") :| ("Guard2" :? "State1"))

-- type MyStateGraphDSL4 = MkStateGraphDSL
--   :* ("State1" :@ "Msg1" :> "State2")
--   :*
--     ( "State2" :@ "Msg2"
--         :>> "Guard1" :? "State3"
--         :|| "Guard2" :? "State1"
--         :|| "Guard3" :? "State4"
--     )
--   :*
--     ( "State3" :@ "Msg3"
--         :>> "Guard1" :? "State1"
--         :|| "Guard2" :? "State2"
--         :|| "Guard3" :? "State4"
--     )

data T a b
data G a b

data S a b

infixl 9 type T as :?
infixl 7 type G as :||
infixl 7 type S as :>>

infixl 5 type T as :>

-- update :: Generically Msg -> Generically State -> Generically State
-- update = mkUpdate @MyStateGraph $
--   unit
--     & match @"State1" @"Msg1" (\msg state -> return @"State2" { bar: "" })
--     & match @"State2" @"Msg2"
--         ( \msg state ->
--             if true then
--               returnVia @"Guard1" @"State3" { baz: false }
--             else
--               returnVia @"Guard1" @"State1" { foo: 0 }
--         )
--     & match @"State1" @"Msg1" (\msg state -> return @"State2" { bar: "" })

-- _match :: forall t0 t1 t2 @t3 @t4. (t0 -> t1 -> t2) -> Match t3 t4 t0 t1 t2
-- _match f = Match f

-- _return :: forall (@sym ∷ Symbol) (a ∷ Type) (r1 ∷ Row Type) (r2 ∷ Row Type). Cons sym a r1 r2 ⇒ IsSymbol sym ⇒ a → Variant r2
-- _return v = V.inj (Proxy :: _ sym) v

-- infixl 5 Tuple as &