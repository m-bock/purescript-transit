module Examples.Circle where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Reflectable (reflectType)
import Data.Variant (Variant)
import Effect (Effect)
import Node.Encoding (Encoding(..))
import Node.FS.Sync as FS
import Transit (type (:*), type (:@), type (>|), Transit, match, mkUpdate, return, TransitCore)
import Transit.Render.Graphviz as TransitGraphviz
import Transit.Render.Theme (themeGradientDark)
import Transit.VariantUtils (v)
import Type.Proxy (Proxy(..))

type State = Variant
  ( "State001" :: {}
  , "State002" :: {}
  , "State003" :: {}
  , "State004" :: {}
  , "State005" :: {}
  , "State006" :: {}
  , "State007" :: {}
  , "State008" :: {}
  , "State009" :: {}
  , "State010" :: {}
  )

init :: State
init = v @"State001"

type Msg = Variant
  ( "Msg001" :: {}
  , "Msg002" :: {}
  , "Msg003" :: {}
  , "Msg004" :: {}
  , "Msg005" :: {}
  , "Msg006" :: {}
  , "Msg007" :: {}
  , "Msg008" :: {}
  , "Msg009" :: {}
  , "Msg010" :: {}
  )

type CircleTransit =
  Transit
    :* ("State001" :@ "Msg001" >| "State002")
    :* ("State002" :@ "Msg002" >| "State003")
    :* ("State003" :@ "Msg003" >| "State004")
    :* ("State004" :@ "Msg004" >| "State005")
    :* ("State005" :@ "Msg005" >| "State006")
    :* ("State006" :@ "Msg006" >| "State007")
    :* ("State007" :@ "Msg007" >| "State008")
    :* ("State008" :@ "Msg008" >| "State009")
    :* ("State009" :@ "Msg009" >| "State010")
    :* ("State010" :@ "Msg010" >| "State001")

circleTransit :: TransitCore
circleTransit = reflectType (Proxy @CircleTransit)

update :: State -> Msg -> State
update = mkUpdate @CircleTransit
  (match @"State001" @"Msg001" \_ _ -> return @"State002")
  (match @"State002" @"Msg002" \_ _ -> return @"State003")
  (match @"State003" @"Msg003" \_ _ -> return @"State004")
  (match @"State004" @"Msg004" \_ _ -> return @"State005")
  (match @"State005" @"Msg005" \_ _ -> return @"State006")
  (match @"State006" @"Msg006" \_ _ -> return @"State007")
  (match @"State007" @"Msg007" \_ _ -> return @"State008")
  (match @"State008" @"Msg008" \_ _ -> return @"State009")
  (match @"State009" @"Msg009" \_ _ -> return @"State010")
  (match @"State010" @"Msg010" \_ _ -> return @"State001")

main :: Effect Unit
main = do
  FS.writeTextFile UTF8 "renders/circle.dot"
    ( TransitGraphviz.generate circleTransit _
        { theme = themeGradientDark
        , globalAttrsRaw = Just "layout=circo;"
        }
    )
