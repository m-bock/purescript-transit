module Examples.Classic.BridgesKoenigsberg (State(..), Msg(..), update, spec) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Examples.Common (assertWalk, (~>))
import Test.Spec (Spec, describe, it)

data Msg
  = Cross_a
  | Cross_b
  | Cross_c
  | Cross_d
  | Cross_e
  | Cross_f
  | Cross_g

derive instance Eq Msg
derive instance Ord Msg

data State
  = LandA
  | LandB
  | LandC
  | LandD

derive instance Eq State
derive instance Generic State _

instance Show State where
  show = genericShow

update :: State -> Msg -> State
update state msg = case state, msg of
  LandA, Cross_a -> LandB
  LandB, Cross_a -> LandA
  LandA, Cross_b -> LandB
  LandB, Cross_b -> LandA
  LandA, Cross_c -> LandC
  LandC, Cross_c -> LandA
  LandA, Cross_d -> LandC
  LandC, Cross_d -> LandA
  LandA, Cross_e -> LandD
  LandD, Cross_e -> LandA
  LandB, Cross_f -> LandD
  LandD, Cross_f -> LandB
  LandC, Cross_g -> LandD
  LandD, Cross_g -> LandC
  _, _ -> state

spec1 :: Spec Unit
spec1 = do
  it "should follow the walk and visit the expected intermediate states" do
    assertWalk update LandA
      [ Cross_a ~> LandB
      , Cross_f ~> LandD
      , Cross_g ~> LandC
      , Cross_c ~> LandA
      , Cross_e ~> LandD
      , Cross_g ~> LandC
      , Cross_d ~> LandA
      , Cross_b ~> LandB
      ]

spec :: Spec Unit
spec = do
  describe "BridgesKoenigsberg" do
    spec1
