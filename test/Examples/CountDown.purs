module Examples.CountDown where

import Prelude

import Data.Foldable (foldl)
import Data.Reflectable (reflectType)
import Data.Variant (Variant)
import Effect (Effect)
import Node.Encoding (Encoding(..))
import Node.FS.Sync as FS
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Transit (type (:*), type (:@), type (>|), Transit, TransitCore, match, mkUpdate, return)
import Transit.Render.Graphviz as TransitGraphviz
import Transit.VariantUtils (v)
import Type.Proxy (Proxy(..))

type State = Variant
  ( "Idle" :: {}
  , "Counting" :: { count :: Int }
  , "Done" :: {}
  )

type Msg = Variant
  ( "Start" :: { initialCount :: Int }
  , "Tick" :: {}
  , "Reset" :: {}
  )

type CountDownTransit =
  Transit
    :* ("Idle" :@ "Start" >| "Counting")
    :* ("Done" :@ "Reset" >| "Idle")
    :*
      ( "Counting" :@ "Tick"
          >| "Counting"
          >| "Done"
      )

update :: State -> Msg -> State
update = mkUpdate @CountDownTransit
  ( match @"Idle" @"Start" \_ msg ->
      return @"Counting" { count: msg.initialCount }
  )
  ( match @"Done" @"Reset" \_ _ ->
      return @"Idle"
  )
  ( match @"Counting" @"Tick" \state _ ->
      let
        nextCount = state.count - 1
      in
        if nextCount == 0 then
          return @"Done"
        else
          return @"Counting" { count: nextCount }
  )

spec :: Spec Unit
spec = do
  describe "CountDown" do
    it "should follow the walk and end in the Done state" do

      let
        init :: State
        init = v @"Idle"

        walk :: Array Msg
        walk =
          [ v @"Start" { initialCount: 3 }
          , v @"Tick"
          , v @"Tick"
          , v @"Tick"
          ]

        finalState :: State
        finalState = foldl update init walk

      finalState `shouldEqual` v @"Done"

countDownTransit :: TransitCore
countDownTransit = reflectType (Proxy @CountDownTransit)

main :: Effect Unit
main = do
  let
    graph :: String
    graph = TransitGraphviz.generate_ countDownTransit

  FS.writeTextFile UTF8 "renders/count-down.dot" graph
