module Examples.Classic.BridgesKoenigsberg (State(..), Msg(..), update) where

import Prelude

import Data.Maybe (Maybe(..))

--------------------------------------------------------------------------------
--- Classic Approach
--------------------------------------------------------------------------------

data Msg = Cross_a | Cross_b | Cross_c | Cross_d | Cross_e | Cross_f | Cross_g

derive instance Eq Msg
derive instance Ord Msg

data State = LandA | LandB | LandC | LandD

derive instance Eq State

update :: State -> Msg -> Maybe State
update state msg = case state, msg of
  LandA, Cross_a -> Just LandB
  LandB, Cross_a -> Just LandA
  LandA, Cross_b -> Just LandB
  LandB, Cross_b -> Just LandA
  LandA, Cross_c -> Just LandC
  LandC, Cross_c -> Just LandA
  LandA, Cross_d -> Just LandC
  LandC, Cross_d -> Just LandA
  LandA, Cross_e -> Just LandD
  LandD, Cross_e -> Just LandA
  LandB, Cross_f -> Just LandD
  LandD, Cross_f -> Just LandB
  LandC, Cross_g -> Just LandD
  LandD, Cross_g -> Just LandC
  _, _ -> Nothing

