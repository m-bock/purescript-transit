module Examples.DoorReadme where

import Prelude

import Data.Reflectable (reflectType)
import Data.Variant (Variant)
import Effect (Effect)
import Node.Encoding (Encoding(..))
import Node.FS.Sync as FS
import Transit (type (:*), type (:@), type (>|), Transit, TransitCore, match, mkUpdate, return)
import Transit.Render.Graphviz as TransitGraphviz
import Type.Prelude (Proxy(..))

type State = Variant
  ( "DoorOpen" :: {}
  , "DoorClosed" :: {}
  )

type Msg = Variant
  ( "Close" :: {}
  , "Open" :: {}
  )

type DoorTransit =
  Transit
    :* ("DoorOpen" :@ "Close" >| "DoorClosed")
    :* ("DoorClosed" :@ "Open" >| "DoorOpen")

update :: State -> Msg -> State
update = mkUpdate @DoorTransit
  (match @"DoorOpen" @"Close" \_ _ -> return @"DoorClosed")
  (match @"DoorClosed" @"Open" \_ _ -> return @"DoorOpen")

doorTransit :: TransitCore
doorTransit = reflectType (Proxy @DoorTransit)

main :: Effect Unit
main =
  FS.writeTextFile UTF8 "renders/door-readme.dot"
    (TransitGraphviz.generate_ doorTransit)

