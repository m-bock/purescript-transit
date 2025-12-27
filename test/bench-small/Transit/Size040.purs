module Test.BenchSmall.Transit.Size040 where

import Prelude

import Data.Tuple.Nested (type (/\), (/\))
import Data.Variant (Variant)
import Data.Variant.Internal (VariantRep(..))
import Transit (type (:*), type (:@), type (>|), Transit, match, mkUpdate, return)
import Transit.VariantUtils (v)
import Unsafe.Coerce (unsafeCoerce)

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
  , "State011" :: {}
  , "State012" :: {}
  , "State013" :: {}
  , "State014" :: {}
  , "State015" :: {}
  , "State016" :: {}
  , "State017" :: {}
  , "State018" :: {}
  , "State019" :: {}
  , "State020" :: {}
  , "State021" :: {}
  , "State022" :: {}
  , "State023" :: {}
  , "State024" :: {}
  , "State025" :: {}
  , "State026" :: {}
  , "State027" :: {}
  , "State028" :: {}
  , "State029" :: {}
  , "State030" :: {}
  , "State031" :: {}
  , "State032" :: {}
  , "State033" :: {}
  , "State034" :: {}
  , "State035" :: {}
  , "State036" :: {}
  , "State037" :: {}
  , "State038" :: {}
  , "State039" :: {}
  , "State040" :: {}
  )

printState :: State -> String
printState v = t
  where
  VariantRep { type: t } = unsafeCoerce v

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
  , "Msg011" :: {}
  , "Msg012" :: {}
  , "Msg013" :: {}
  , "Msg014" :: {}
  , "Msg015" :: {}
  , "Msg016" :: {}
  , "Msg017" :: {}
  , "Msg018" :: {}
  , "Msg019" :: {}
  , "Msg020" :: {}
  , "Msg021" :: {}
  , "Msg022" :: {}
  , "Msg023" :: {}
  , "Msg024" :: {}
  , "Msg025" :: {}
  , "Msg026" :: {}
  , "Msg027" :: {}
  , "Msg028" :: {}
  , "Msg029" :: {}
  , "Msg030" :: {}
  , "Msg031" :: {}
  , "Msg032" :: {}
  , "Msg033" :: {}
  , "Msg034" :: {}
  , "Msg035" :: {}
  , "Msg036" :: {}
  , "Msg037" :: {}
  , "Msg038" :: {}
  , "Msg039" :: {}
  , "Msg040" :: {}
  )

printMsg :: Msg -> String
printMsg v = t
  where
  VariantRep { type: t } = unsafeCoerce v

type BenchTransit =
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
    :* ("State010" :@ "Msg010" >| "State011")
    :* ("State011" :@ "Msg011" >| "State012")
    :* ("State012" :@ "Msg012" >| "State013")
    :* ("State013" :@ "Msg013" >| "State014")
    :* ("State014" :@ "Msg014" >| "State015")
    :* ("State015" :@ "Msg015" >| "State016")
    :* ("State016" :@ "Msg016" >| "State017")
    :* ("State017" :@ "Msg017" >| "State018")
    :* ("State018" :@ "Msg018" >| "State019")
    :* ("State019" :@ "Msg019" >| "State020")
    :* ("State020" :@ "Msg020" >| "State021")
    :* ("State021" :@ "Msg021" >| "State022")
    :* ("State022" :@ "Msg022" >| "State023")
    :* ("State023" :@ "Msg023" >| "State024")
    :* ("State024" :@ "Msg024" >| "State025")
    :* ("State025" :@ "Msg025" >| "State026")
    :* ("State026" :@ "Msg026" >| "State027")
    :* ("State027" :@ "Msg027" >| "State028")
    :* ("State028" :@ "Msg028" >| "State029")
    :* ("State029" :@ "Msg029" >| "State030")
    :* ("State030" :@ "Msg030" >| "State031")
    :* ("State031" :@ "Msg031" >| "State032")
    :* ("State032" :@ "Msg032" >| "State033")
    :* ("State033" :@ "Msg033" >| "State034")
    :* ("State034" :@ "Msg034" >| "State035")
    :* ("State035" :@ "Msg035" >| "State036")
    :* ("State036" :@ "Msg036" >| "State037")
    :* ("State037" :@ "Msg037" >| "State038")
    :* ("State038" :@ "Msg038" >| "State039")
    :* ("State039" :@ "Msg039" >| "State040")
    :* ("State040" :@ "Msg040" >| "State001")

update :: State -> Msg -> State
update = mkUpdate @BenchTransit
  (match @"State001" @"Msg001" \_ _ -> return @"State002")
  (match @"State002" @"Msg002" \_ _ -> return @"State003")
  (match @"State003" @"Msg003" \_ _ -> return @"State004")
  (match @"State004" @"Msg004" \_ _ -> return @"State005")
  (match @"State005" @"Msg005" \_ _ -> return @"State006")
  (match @"State006" @"Msg006" \_ _ -> return @"State007")
  (match @"State007" @"Msg007" \_ _ -> return @"State008")
  (match @"State008" @"Msg008" \_ _ -> return @"State009")
  (match @"State009" @"Msg009" \_ _ -> return @"State010")
  (match @"State010" @"Msg010" \_ _ -> return @"State011")
  (match @"State011" @"Msg011" \_ _ -> return @"State012")
  (match @"State012" @"Msg012" \_ _ -> return @"State013")
  (match @"State013" @"Msg013" \_ _ -> return @"State014")
  (match @"State014" @"Msg014" \_ _ -> return @"State015")
  (match @"State015" @"Msg015" \_ _ -> return @"State016")
  (match @"State016" @"Msg016" \_ _ -> return @"State017")
  (match @"State017" @"Msg017" \_ _ -> return @"State018")
  (match @"State018" @"Msg018" \_ _ -> return @"State019")
  (match @"State019" @"Msg019" \_ _ -> return @"State020")
  (match @"State020" @"Msg020" \_ _ -> return @"State021")
  (match @"State021" @"Msg021" \_ _ -> return @"State022")
  (match @"State022" @"Msg022" \_ _ -> return @"State023")
  (match @"State023" @"Msg023" \_ _ -> return @"State024")
  (match @"State024" @"Msg024" \_ _ -> return @"State025")
  (match @"State025" @"Msg025" \_ _ -> return @"State026")
  (match @"State026" @"Msg026" \_ _ -> return @"State027")
  (match @"State027" @"Msg027" \_ _ -> return @"State028")
  (match @"State028" @"Msg028" \_ _ -> return @"State029")
  (match @"State029" @"Msg029" \_ _ -> return @"State030")
  (match @"State030" @"Msg030" \_ _ -> return @"State031")
  (match @"State031" @"Msg031" \_ _ -> return @"State032")
  (match @"State032" @"Msg032" \_ _ -> return @"State033")
  (match @"State033" @"Msg033" \_ _ -> return @"State034")
  (match @"State034" @"Msg034" \_ _ -> return @"State035")
  (match @"State035" @"Msg035" \_ _ -> return @"State036")
  (match @"State036" @"Msg036" \_ _ -> return @"State037")
  (match @"State037" @"Msg037" \_ _ -> return @"State038")
  (match @"State038" @"Msg038" \_ _ -> return @"State039")
  (match @"State039" @"Msg039" \_ _ -> return @"State040")
  (match @"State040" @"Msg040" \_ _ -> return @"State001")

walk :: Array (Msg /\ State)
walk =
  [ v @"Msg001" /\ v @"State002"
  , v @"Msg002" /\ v @"State003"
  , v @"Msg003" /\ v @"State004"
  , v @"Msg004" /\ v @"State005"
  , v @"Msg005" /\ v @"State006"
  , v @"Msg006" /\ v @"State007"
  , v @"Msg007" /\ v @"State008"
  , v @"Msg008" /\ v @"State009"
  , v @"Msg009" /\ v @"State010"
  , v @"Msg010" /\ v @"State011"
  , v @"Msg011" /\ v @"State012"
  , v @"Msg012" /\ v @"State013"
  , v @"Msg013" /\ v @"State014"
  , v @"Msg014" /\ v @"State015"
  , v @"Msg015" /\ v @"State016"
  , v @"Msg016" /\ v @"State017"
  , v @"Msg017" /\ v @"State018"
  , v @"Msg018" /\ v @"State019"
  , v @"Msg019" /\ v @"State020"
  , v @"Msg020" /\ v @"State021"
  , v @"Msg021" /\ v @"State022"
  , v @"Msg022" /\ v @"State023"
  , v @"Msg023" /\ v @"State024"
  , v @"Msg024" /\ v @"State025"
  , v @"Msg025" /\ v @"State026"
  , v @"Msg026" /\ v @"State027"
  , v @"Msg027" /\ v @"State028"
  , v @"Msg028" /\ v @"State029"
  , v @"Msg029" /\ v @"State030"
  , v @"Msg030" /\ v @"State031"
  , v @"Msg031" /\ v @"State032"
  , v @"Msg032" /\ v @"State033"
  , v @"Msg033" /\ v @"State034"
  , v @"Msg034" /\ v @"State035"
  , v @"Msg035" /\ v @"State036"
  , v @"Msg036" /\ v @"State037"
  , v @"Msg037" /\ v @"State038"
  , v @"Msg038" /\ v @"State039"
  , v @"Msg039" /\ v @"State040"
  , v @"Msg040" /\ v @"State001"
  ]
