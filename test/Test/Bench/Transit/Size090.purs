module Test.Bench.Transit.Size090 where

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
  , "State041" :: {}
  , "State042" :: {}
  , "State043" :: {}
  , "State044" :: {}
  , "State045" :: {}
  , "State046" :: {}
  , "State047" :: {}
  , "State048" :: {}
  , "State049" :: {}
  , "State050" :: {}
  , "State051" :: {}
  , "State052" :: {}
  , "State053" :: {}
  , "State054" :: {}
  , "State055" :: {}
  , "State056" :: {}
  , "State057" :: {}
  , "State058" :: {}
  , "State059" :: {}
  , "State060" :: {}
  , "State061" :: {}
  , "State062" :: {}
  , "State063" :: {}
  , "State064" :: {}
  , "State065" :: {}
  , "State066" :: {}
  , "State067" :: {}
  , "State068" :: {}
  , "State069" :: {}
  , "State070" :: {}
  , "State071" :: {}
  , "State072" :: {}
  , "State073" :: {}
  , "State074" :: {}
  , "State075" :: {}
  , "State076" :: {}
  , "State077" :: {}
  , "State078" :: {}
  , "State079" :: {}
  , "State080" :: {}
  , "State081" :: {}
  , "State082" :: {}
  , "State083" :: {}
  , "State084" :: {}
  , "State085" :: {}
  , "State086" :: {}
  , "State087" :: {}
  , "State088" :: {}
  , "State089" :: {}
  , "State090" :: {}
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
  , "Msg041" :: {}
  , "Msg042" :: {}
  , "Msg043" :: {}
  , "Msg044" :: {}
  , "Msg045" :: {}
  , "Msg046" :: {}
  , "Msg047" :: {}
  , "Msg048" :: {}
  , "Msg049" :: {}
  , "Msg050" :: {}
  , "Msg051" :: {}
  , "Msg052" :: {}
  , "Msg053" :: {}
  , "Msg054" :: {}
  , "Msg055" :: {}
  , "Msg056" :: {}
  , "Msg057" :: {}
  , "Msg058" :: {}
  , "Msg059" :: {}
  , "Msg060" :: {}
  , "Msg061" :: {}
  , "Msg062" :: {}
  , "Msg063" :: {}
  , "Msg064" :: {}
  , "Msg065" :: {}
  , "Msg066" :: {}
  , "Msg067" :: {}
  , "Msg068" :: {}
  , "Msg069" :: {}
  , "Msg070" :: {}
  , "Msg071" :: {}
  , "Msg072" :: {}
  , "Msg073" :: {}
  , "Msg074" :: {}
  , "Msg075" :: {}
  , "Msg076" :: {}
  , "Msg077" :: {}
  , "Msg078" :: {}
  , "Msg079" :: {}
  , "Msg080" :: {}
  , "Msg081" :: {}
  , "Msg082" :: {}
  , "Msg083" :: {}
  , "Msg084" :: {}
  , "Msg085" :: {}
  , "Msg086" :: {}
  , "Msg087" :: {}
  , "Msg088" :: {}
  , "Msg089" :: {}
  , "Msg090" :: {}
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
    :* ("State040" :@ "Msg040" >| "State041")
    :* ("State041" :@ "Msg041" >| "State042")
    :* ("State042" :@ "Msg042" >| "State043")
    :* ("State043" :@ "Msg043" >| "State044")
    :* ("State044" :@ "Msg044" >| "State045")
    :* ("State045" :@ "Msg045" >| "State046")
    :* ("State046" :@ "Msg046" >| "State047")
    :* ("State047" :@ "Msg047" >| "State048")
    :* ("State048" :@ "Msg048" >| "State049")
    :* ("State049" :@ "Msg049" >| "State050")
    :* ("State050" :@ "Msg050" >| "State051")
    :* ("State051" :@ "Msg051" >| "State052")
    :* ("State052" :@ "Msg052" >| "State053")
    :* ("State053" :@ "Msg053" >| "State054")
    :* ("State054" :@ "Msg054" >| "State055")
    :* ("State055" :@ "Msg055" >| "State056")
    :* ("State056" :@ "Msg056" >| "State057")
    :* ("State057" :@ "Msg057" >| "State058")
    :* ("State058" :@ "Msg058" >| "State059")
    :* ("State059" :@ "Msg059" >| "State060")
    :* ("State060" :@ "Msg060" >| "State061")
    :* ("State061" :@ "Msg061" >| "State062")
    :* ("State062" :@ "Msg062" >| "State063")
    :* ("State063" :@ "Msg063" >| "State064")
    :* ("State064" :@ "Msg064" >| "State065")
    :* ("State065" :@ "Msg065" >| "State066")
    :* ("State066" :@ "Msg066" >| "State067")
    :* ("State067" :@ "Msg067" >| "State068")
    :* ("State068" :@ "Msg068" >| "State069")
    :* ("State069" :@ "Msg069" >| "State070")
    :* ("State070" :@ "Msg070" >| "State071")
    :* ("State071" :@ "Msg071" >| "State072")
    :* ("State072" :@ "Msg072" >| "State073")
    :* ("State073" :@ "Msg073" >| "State074")
    :* ("State074" :@ "Msg074" >| "State075")
    :* ("State075" :@ "Msg075" >| "State076")
    :* ("State076" :@ "Msg076" >| "State077")
    :* ("State077" :@ "Msg077" >| "State078")
    :* ("State078" :@ "Msg078" >| "State079")
    :* ("State079" :@ "Msg079" >| "State080")
    :* ("State080" :@ "Msg080" >| "State081")
    :* ("State081" :@ "Msg081" >| "State082")
    :* ("State082" :@ "Msg082" >| "State083")
    :* ("State083" :@ "Msg083" >| "State084")
    :* ("State084" :@ "Msg084" >| "State085")
    :* ("State085" :@ "Msg085" >| "State086")
    :* ("State086" :@ "Msg086" >| "State087")
    :* ("State087" :@ "Msg087" >| "State088")
    :* ("State088" :@ "Msg088" >| "State089")
    :* ("State089" :@ "Msg089" >| "State090")
    :* ("State090" :@ "Msg090" >| "State001")

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
  (match @"State040" @"Msg040" \_ _ -> return @"State041")
  (match @"State041" @"Msg041" \_ _ -> return @"State042")
  (match @"State042" @"Msg042" \_ _ -> return @"State043")
  (match @"State043" @"Msg043" \_ _ -> return @"State044")
  (match @"State044" @"Msg044" \_ _ -> return @"State045")
  (match @"State045" @"Msg045" \_ _ -> return @"State046")
  (match @"State046" @"Msg046" \_ _ -> return @"State047")
  (match @"State047" @"Msg047" \_ _ -> return @"State048")
  (match @"State048" @"Msg048" \_ _ -> return @"State049")
  (match @"State049" @"Msg049" \_ _ -> return @"State050")
  (match @"State050" @"Msg050" \_ _ -> return @"State051")
  (match @"State051" @"Msg051" \_ _ -> return @"State052")
  (match @"State052" @"Msg052" \_ _ -> return @"State053")
  (match @"State053" @"Msg053" \_ _ -> return @"State054")
  (match @"State054" @"Msg054" \_ _ -> return @"State055")
  (match @"State055" @"Msg055" \_ _ -> return @"State056")
  (match @"State056" @"Msg056" \_ _ -> return @"State057")
  (match @"State057" @"Msg057" \_ _ -> return @"State058")
  (match @"State058" @"Msg058" \_ _ -> return @"State059")
  (match @"State059" @"Msg059" \_ _ -> return @"State060")
  (match @"State060" @"Msg060" \_ _ -> return @"State061")
  (match @"State061" @"Msg061" \_ _ -> return @"State062")
  (match @"State062" @"Msg062" \_ _ -> return @"State063")
  (match @"State063" @"Msg063" \_ _ -> return @"State064")
  (match @"State064" @"Msg064" \_ _ -> return @"State065")
  (match @"State065" @"Msg065" \_ _ -> return @"State066")
  (match @"State066" @"Msg066" \_ _ -> return @"State067")
  (match @"State067" @"Msg067" \_ _ -> return @"State068")
  (match @"State068" @"Msg068" \_ _ -> return @"State069")
  (match @"State069" @"Msg069" \_ _ -> return @"State070")
  (match @"State070" @"Msg070" \_ _ -> return @"State071")
  (match @"State071" @"Msg071" \_ _ -> return @"State072")
  (match @"State072" @"Msg072" \_ _ -> return @"State073")
  (match @"State073" @"Msg073" \_ _ -> return @"State074")
  (match @"State074" @"Msg074" \_ _ -> return @"State075")
  (match @"State075" @"Msg075" \_ _ -> return @"State076")
  (match @"State076" @"Msg076" \_ _ -> return @"State077")
  (match @"State077" @"Msg077" \_ _ -> return @"State078")
  (match @"State078" @"Msg078" \_ _ -> return @"State079")
  (match @"State079" @"Msg079" \_ _ -> return @"State080")
  (match @"State080" @"Msg080" \_ _ -> return @"State081")
  (match @"State081" @"Msg081" \_ _ -> return @"State082")
  (match @"State082" @"Msg082" \_ _ -> return @"State083")
  (match @"State083" @"Msg083" \_ _ -> return @"State084")
  (match @"State084" @"Msg084" \_ _ -> return @"State085")
  (match @"State085" @"Msg085" \_ _ -> return @"State086")
  (match @"State086" @"Msg086" \_ _ -> return @"State087")
  (match @"State087" @"Msg087" \_ _ -> return @"State088")
  (match @"State088" @"Msg088" \_ _ -> return @"State089")
  (match @"State089" @"Msg089" \_ _ -> return @"State090")
  (match @"State090" @"Msg090" \_ _ -> return @"State001")

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
  , v @"Msg040" /\ v @"State041"
  , v @"Msg041" /\ v @"State042"
  , v @"Msg042" /\ v @"State043"
  , v @"Msg043" /\ v @"State044"
  , v @"Msg044" /\ v @"State045"
  , v @"Msg045" /\ v @"State046"
  , v @"Msg046" /\ v @"State047"
  , v @"Msg047" /\ v @"State048"
  , v @"Msg048" /\ v @"State049"
  , v @"Msg049" /\ v @"State050"
  , v @"Msg050" /\ v @"State051"
  , v @"Msg051" /\ v @"State052"
  , v @"Msg052" /\ v @"State053"
  , v @"Msg053" /\ v @"State054"
  , v @"Msg054" /\ v @"State055"
  , v @"Msg055" /\ v @"State056"
  , v @"Msg056" /\ v @"State057"
  , v @"Msg057" /\ v @"State058"
  , v @"Msg058" /\ v @"State059"
  , v @"Msg059" /\ v @"State060"
  , v @"Msg060" /\ v @"State061"
  , v @"Msg061" /\ v @"State062"
  , v @"Msg062" /\ v @"State063"
  , v @"Msg063" /\ v @"State064"
  , v @"Msg064" /\ v @"State065"
  , v @"Msg065" /\ v @"State066"
  , v @"Msg066" /\ v @"State067"
  , v @"Msg067" /\ v @"State068"
  , v @"Msg068" /\ v @"State069"
  , v @"Msg069" /\ v @"State070"
  , v @"Msg070" /\ v @"State071"
  , v @"Msg071" /\ v @"State072"
  , v @"Msg072" /\ v @"State073"
  , v @"Msg073" /\ v @"State074"
  , v @"Msg074" /\ v @"State075"
  , v @"Msg075" /\ v @"State076"
  , v @"Msg076" /\ v @"State077"
  , v @"Msg077" /\ v @"State078"
  , v @"Msg078" /\ v @"State079"
  , v @"Msg079" /\ v @"State080"
  , v @"Msg080" /\ v @"State081"
  , v @"Msg081" /\ v @"State082"
  , v @"Msg082" /\ v @"State083"
  , v @"Msg083" /\ v @"State084"
  , v @"Msg084" /\ v @"State085"
  , v @"Msg085" /\ v @"State086"
  , v @"Msg086" /\ v @"State087"
  , v @"Msg087" /\ v @"State088"
  , v @"Msg088" /\ v @"State089"
  , v @"Msg089" /\ v @"State090"
  , v @"Msg090" /\ v @"State001"
  ]
