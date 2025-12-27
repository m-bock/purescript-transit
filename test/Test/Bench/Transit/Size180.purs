module Test.Bench.Transit.Size180 where

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
  , "State091" :: {}
  , "State092" :: {}
  , "State093" :: {}
  , "State094" :: {}
  , "State095" :: {}
  , "State096" :: {}
  , "State097" :: {}
  , "State098" :: {}
  , "State099" :: {}
  , "State100" :: {}
  , "State101" :: {}
  , "State102" :: {}
  , "State103" :: {}
  , "State104" :: {}
  , "State105" :: {}
  , "State106" :: {}
  , "State107" :: {}
  , "State108" :: {}
  , "State109" :: {}
  , "State110" :: {}
  , "State111" :: {}
  , "State112" :: {}
  , "State113" :: {}
  , "State114" :: {}
  , "State115" :: {}
  , "State116" :: {}
  , "State117" :: {}
  , "State118" :: {}
  , "State119" :: {}
  , "State120" :: {}
  , "State121" :: {}
  , "State122" :: {}
  , "State123" :: {}
  , "State124" :: {}
  , "State125" :: {}
  , "State126" :: {}
  , "State127" :: {}
  , "State128" :: {}
  , "State129" :: {}
  , "State130" :: {}
  , "State131" :: {}
  , "State132" :: {}
  , "State133" :: {}
  , "State134" :: {}
  , "State135" :: {}
  , "State136" :: {}
  , "State137" :: {}
  , "State138" :: {}
  , "State139" :: {}
  , "State140" :: {}
  , "State141" :: {}
  , "State142" :: {}
  , "State143" :: {}
  , "State144" :: {}
  , "State145" :: {}
  , "State146" :: {}
  , "State147" :: {}
  , "State148" :: {}
  , "State149" :: {}
  , "State150" :: {}
  , "State151" :: {}
  , "State152" :: {}
  , "State153" :: {}
  , "State154" :: {}
  , "State155" :: {}
  , "State156" :: {}
  , "State157" :: {}
  , "State158" :: {}
  , "State159" :: {}
  , "State160" :: {}
  , "State161" :: {}
  , "State162" :: {}
  , "State163" :: {}
  , "State164" :: {}
  , "State165" :: {}
  , "State166" :: {}
  , "State167" :: {}
  , "State168" :: {}
  , "State169" :: {}
  , "State170" :: {}
  , "State171" :: {}
  , "State172" :: {}
  , "State173" :: {}
  , "State174" :: {}
  , "State175" :: {}
  , "State176" :: {}
  , "State177" :: {}
  , "State178" :: {}
  , "State179" :: {}
  , "State180" :: {}
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
  , "Msg091" :: {}
  , "Msg092" :: {}
  , "Msg093" :: {}
  , "Msg094" :: {}
  , "Msg095" :: {}
  , "Msg096" :: {}
  , "Msg097" :: {}
  , "Msg098" :: {}
  , "Msg099" :: {}
  , "Msg100" :: {}
  , "Msg101" :: {}
  , "Msg102" :: {}
  , "Msg103" :: {}
  , "Msg104" :: {}
  , "Msg105" :: {}
  , "Msg106" :: {}
  , "Msg107" :: {}
  , "Msg108" :: {}
  , "Msg109" :: {}
  , "Msg110" :: {}
  , "Msg111" :: {}
  , "Msg112" :: {}
  , "Msg113" :: {}
  , "Msg114" :: {}
  , "Msg115" :: {}
  , "Msg116" :: {}
  , "Msg117" :: {}
  , "Msg118" :: {}
  , "Msg119" :: {}
  , "Msg120" :: {}
  , "Msg121" :: {}
  , "Msg122" :: {}
  , "Msg123" :: {}
  , "Msg124" :: {}
  , "Msg125" :: {}
  , "Msg126" :: {}
  , "Msg127" :: {}
  , "Msg128" :: {}
  , "Msg129" :: {}
  , "Msg130" :: {}
  , "Msg131" :: {}
  , "Msg132" :: {}
  , "Msg133" :: {}
  , "Msg134" :: {}
  , "Msg135" :: {}
  , "Msg136" :: {}
  , "Msg137" :: {}
  , "Msg138" :: {}
  , "Msg139" :: {}
  , "Msg140" :: {}
  , "Msg141" :: {}
  , "Msg142" :: {}
  , "Msg143" :: {}
  , "Msg144" :: {}
  , "Msg145" :: {}
  , "Msg146" :: {}
  , "Msg147" :: {}
  , "Msg148" :: {}
  , "Msg149" :: {}
  , "Msg150" :: {}
  , "Msg151" :: {}
  , "Msg152" :: {}
  , "Msg153" :: {}
  , "Msg154" :: {}
  , "Msg155" :: {}
  , "Msg156" :: {}
  , "Msg157" :: {}
  , "Msg158" :: {}
  , "Msg159" :: {}
  , "Msg160" :: {}
  , "Msg161" :: {}
  , "Msg162" :: {}
  , "Msg163" :: {}
  , "Msg164" :: {}
  , "Msg165" :: {}
  , "Msg166" :: {}
  , "Msg167" :: {}
  , "Msg168" :: {}
  , "Msg169" :: {}
  , "Msg170" :: {}
  , "Msg171" :: {}
  , "Msg172" :: {}
  , "Msg173" :: {}
  , "Msg174" :: {}
  , "Msg175" :: {}
  , "Msg176" :: {}
  , "Msg177" :: {}
  , "Msg178" :: {}
  , "Msg179" :: {}
  , "Msg180" :: {}
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
    :* ("State090" :@ "Msg090" >| "State091")
    :* ("State091" :@ "Msg091" >| "State092")
    :* ("State092" :@ "Msg092" >| "State093")
    :* ("State093" :@ "Msg093" >| "State094")
    :* ("State094" :@ "Msg094" >| "State095")
    :* ("State095" :@ "Msg095" >| "State096")
    :* ("State096" :@ "Msg096" >| "State097")
    :* ("State097" :@ "Msg097" >| "State098")
    :* ("State098" :@ "Msg098" >| "State099")
    :* ("State099" :@ "Msg099" >| "State100")
    :* ("State100" :@ "Msg100" >| "State101")
    :* ("State101" :@ "Msg101" >| "State102")
    :* ("State102" :@ "Msg102" >| "State103")
    :* ("State103" :@ "Msg103" >| "State104")
    :* ("State104" :@ "Msg104" >| "State105")
    :* ("State105" :@ "Msg105" >| "State106")
    :* ("State106" :@ "Msg106" >| "State107")
    :* ("State107" :@ "Msg107" >| "State108")
    :* ("State108" :@ "Msg108" >| "State109")
    :* ("State109" :@ "Msg109" >| "State110")
    :* ("State110" :@ "Msg110" >| "State111")
    :* ("State111" :@ "Msg111" >| "State112")
    :* ("State112" :@ "Msg112" >| "State113")
    :* ("State113" :@ "Msg113" >| "State114")
    :* ("State114" :@ "Msg114" >| "State115")
    :* ("State115" :@ "Msg115" >| "State116")
    :* ("State116" :@ "Msg116" >| "State117")
    :* ("State117" :@ "Msg117" >| "State118")
    :* ("State118" :@ "Msg118" >| "State119")
    :* ("State119" :@ "Msg119" >| "State120")
    :* ("State120" :@ "Msg120" >| "State121")
    :* ("State121" :@ "Msg121" >| "State122")
    :* ("State122" :@ "Msg122" >| "State123")
    :* ("State123" :@ "Msg123" >| "State124")
    :* ("State124" :@ "Msg124" >| "State125")
    :* ("State125" :@ "Msg125" >| "State126")
    :* ("State126" :@ "Msg126" >| "State127")
    :* ("State127" :@ "Msg127" >| "State128")
    :* ("State128" :@ "Msg128" >| "State129")
    :* ("State129" :@ "Msg129" >| "State130")
    :* ("State130" :@ "Msg130" >| "State131")
    :* ("State131" :@ "Msg131" >| "State132")
    :* ("State132" :@ "Msg132" >| "State133")
    :* ("State133" :@ "Msg133" >| "State134")
    :* ("State134" :@ "Msg134" >| "State135")
    :* ("State135" :@ "Msg135" >| "State136")
    :* ("State136" :@ "Msg136" >| "State137")
    :* ("State137" :@ "Msg137" >| "State138")
    :* ("State138" :@ "Msg138" >| "State139")
    :* ("State139" :@ "Msg139" >| "State140")
    :* ("State140" :@ "Msg140" >| "State141")
    :* ("State141" :@ "Msg141" >| "State142")
    :* ("State142" :@ "Msg142" >| "State143")
    :* ("State143" :@ "Msg143" >| "State144")
    :* ("State144" :@ "Msg144" >| "State145")
    :* ("State145" :@ "Msg145" >| "State146")
    :* ("State146" :@ "Msg146" >| "State147")
    :* ("State147" :@ "Msg147" >| "State148")
    :* ("State148" :@ "Msg148" >| "State149")
    :* ("State149" :@ "Msg149" >| "State150")
    :* ("State150" :@ "Msg150" >| "State151")
    :* ("State151" :@ "Msg151" >| "State152")
    :* ("State152" :@ "Msg152" >| "State153")
    :* ("State153" :@ "Msg153" >| "State154")
    :* ("State154" :@ "Msg154" >| "State155")
    :* ("State155" :@ "Msg155" >| "State156")
    :* ("State156" :@ "Msg156" >| "State157")
    :* ("State157" :@ "Msg157" >| "State158")
    :* ("State158" :@ "Msg158" >| "State159")
    :* ("State159" :@ "Msg159" >| "State160")
    :* ("State160" :@ "Msg160" >| "State161")
    :* ("State161" :@ "Msg161" >| "State162")
    :* ("State162" :@ "Msg162" >| "State163")
    :* ("State163" :@ "Msg163" >| "State164")
    :* ("State164" :@ "Msg164" >| "State165")
    :* ("State165" :@ "Msg165" >| "State166")
    :* ("State166" :@ "Msg166" >| "State167")
    :* ("State167" :@ "Msg167" >| "State168")
    :* ("State168" :@ "Msg168" >| "State169")
    :* ("State169" :@ "Msg169" >| "State170")
    :* ("State170" :@ "Msg170" >| "State171")
    :* ("State171" :@ "Msg171" >| "State172")
    :* ("State172" :@ "Msg172" >| "State173")
    :* ("State173" :@ "Msg173" >| "State174")
    :* ("State174" :@ "Msg174" >| "State175")
    :* ("State175" :@ "Msg175" >| "State176")
    :* ("State176" :@ "Msg176" >| "State177")
    :* ("State177" :@ "Msg177" >| "State178")
    :* ("State178" :@ "Msg178" >| "State179")
    :* ("State179" :@ "Msg179" >| "State180")
    :* ("State180" :@ "Msg180" >| "State001")

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
  (match @"State090" @"Msg090" \_ _ -> return @"State091")
  (match @"State091" @"Msg091" \_ _ -> return @"State092")
  (match @"State092" @"Msg092" \_ _ -> return @"State093")
  (match @"State093" @"Msg093" \_ _ -> return @"State094")
  (match @"State094" @"Msg094" \_ _ -> return @"State095")
  (match @"State095" @"Msg095" \_ _ -> return @"State096")
  (match @"State096" @"Msg096" \_ _ -> return @"State097")
  (match @"State097" @"Msg097" \_ _ -> return @"State098")
  (match @"State098" @"Msg098" \_ _ -> return @"State099")
  (match @"State099" @"Msg099" \_ _ -> return @"State100")
  (match @"State100" @"Msg100" \_ _ -> return @"State101")
  (match @"State101" @"Msg101" \_ _ -> return @"State102")
  (match @"State102" @"Msg102" \_ _ -> return @"State103")
  (match @"State103" @"Msg103" \_ _ -> return @"State104")
  (match @"State104" @"Msg104" \_ _ -> return @"State105")
  (match @"State105" @"Msg105" \_ _ -> return @"State106")
  (match @"State106" @"Msg106" \_ _ -> return @"State107")
  (match @"State107" @"Msg107" \_ _ -> return @"State108")
  (match @"State108" @"Msg108" \_ _ -> return @"State109")
  (match @"State109" @"Msg109" \_ _ -> return @"State110")
  (match @"State110" @"Msg110" \_ _ -> return @"State111")
  (match @"State111" @"Msg111" \_ _ -> return @"State112")
  (match @"State112" @"Msg112" \_ _ -> return @"State113")
  (match @"State113" @"Msg113" \_ _ -> return @"State114")
  (match @"State114" @"Msg114" \_ _ -> return @"State115")
  (match @"State115" @"Msg115" \_ _ -> return @"State116")
  (match @"State116" @"Msg116" \_ _ -> return @"State117")
  (match @"State117" @"Msg117" \_ _ -> return @"State118")
  (match @"State118" @"Msg118" \_ _ -> return @"State119")
  (match @"State119" @"Msg119" \_ _ -> return @"State120")
  (match @"State120" @"Msg120" \_ _ -> return @"State121")
  (match @"State121" @"Msg121" \_ _ -> return @"State122")
  (match @"State122" @"Msg122" \_ _ -> return @"State123")
  (match @"State123" @"Msg123" \_ _ -> return @"State124")
  (match @"State124" @"Msg124" \_ _ -> return @"State125")
  (match @"State125" @"Msg125" \_ _ -> return @"State126")
  (match @"State126" @"Msg126" \_ _ -> return @"State127")
  (match @"State127" @"Msg127" \_ _ -> return @"State128")
  (match @"State128" @"Msg128" \_ _ -> return @"State129")
  (match @"State129" @"Msg129" \_ _ -> return @"State130")
  (match @"State130" @"Msg130" \_ _ -> return @"State131")
  (match @"State131" @"Msg131" \_ _ -> return @"State132")
  (match @"State132" @"Msg132" \_ _ -> return @"State133")
  (match @"State133" @"Msg133" \_ _ -> return @"State134")
  (match @"State134" @"Msg134" \_ _ -> return @"State135")
  (match @"State135" @"Msg135" \_ _ -> return @"State136")
  (match @"State136" @"Msg136" \_ _ -> return @"State137")
  (match @"State137" @"Msg137" \_ _ -> return @"State138")
  (match @"State138" @"Msg138" \_ _ -> return @"State139")
  (match @"State139" @"Msg139" \_ _ -> return @"State140")
  (match @"State140" @"Msg140" \_ _ -> return @"State141")
  (match @"State141" @"Msg141" \_ _ -> return @"State142")
  (match @"State142" @"Msg142" \_ _ -> return @"State143")
  (match @"State143" @"Msg143" \_ _ -> return @"State144")
  (match @"State144" @"Msg144" \_ _ -> return @"State145")
  (match @"State145" @"Msg145" \_ _ -> return @"State146")
  (match @"State146" @"Msg146" \_ _ -> return @"State147")
  (match @"State147" @"Msg147" \_ _ -> return @"State148")
  (match @"State148" @"Msg148" \_ _ -> return @"State149")
  (match @"State149" @"Msg149" \_ _ -> return @"State150")
  (match @"State150" @"Msg150" \_ _ -> return @"State151")
  (match @"State151" @"Msg151" \_ _ -> return @"State152")
  (match @"State152" @"Msg152" \_ _ -> return @"State153")
  (match @"State153" @"Msg153" \_ _ -> return @"State154")
  (match @"State154" @"Msg154" \_ _ -> return @"State155")
  (match @"State155" @"Msg155" \_ _ -> return @"State156")
  (match @"State156" @"Msg156" \_ _ -> return @"State157")
  (match @"State157" @"Msg157" \_ _ -> return @"State158")
  (match @"State158" @"Msg158" \_ _ -> return @"State159")
  (match @"State159" @"Msg159" \_ _ -> return @"State160")
  (match @"State160" @"Msg160" \_ _ -> return @"State161")
  (match @"State161" @"Msg161" \_ _ -> return @"State162")
  (match @"State162" @"Msg162" \_ _ -> return @"State163")
  (match @"State163" @"Msg163" \_ _ -> return @"State164")
  (match @"State164" @"Msg164" \_ _ -> return @"State165")
  (match @"State165" @"Msg165" \_ _ -> return @"State166")
  (match @"State166" @"Msg166" \_ _ -> return @"State167")
  (match @"State167" @"Msg167" \_ _ -> return @"State168")
  (match @"State168" @"Msg168" \_ _ -> return @"State169")
  (match @"State169" @"Msg169" \_ _ -> return @"State170")
  (match @"State170" @"Msg170" \_ _ -> return @"State171")
  (match @"State171" @"Msg171" \_ _ -> return @"State172")
  (match @"State172" @"Msg172" \_ _ -> return @"State173")
  (match @"State173" @"Msg173" \_ _ -> return @"State174")
  (match @"State174" @"Msg174" \_ _ -> return @"State175")
  (match @"State175" @"Msg175" \_ _ -> return @"State176")
  (match @"State176" @"Msg176" \_ _ -> return @"State177")
  (match @"State177" @"Msg177" \_ _ -> return @"State178")
  (match @"State178" @"Msg178" \_ _ -> return @"State179")
  (match @"State179" @"Msg179" \_ _ -> return @"State180")
  (match @"State180" @"Msg180" \_ _ -> return @"State001")

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
  , v @"Msg090" /\ v @"State091"
  , v @"Msg091" /\ v @"State092"
  , v @"Msg092" /\ v @"State093"
  , v @"Msg093" /\ v @"State094"
  , v @"Msg094" /\ v @"State095"
  , v @"Msg095" /\ v @"State096"
  , v @"Msg096" /\ v @"State097"
  , v @"Msg097" /\ v @"State098"
  , v @"Msg098" /\ v @"State099"
  , v @"Msg099" /\ v @"State100"
  , v @"Msg100" /\ v @"State101"
  , v @"Msg101" /\ v @"State102"
  , v @"Msg102" /\ v @"State103"
  , v @"Msg103" /\ v @"State104"
  , v @"Msg104" /\ v @"State105"
  , v @"Msg105" /\ v @"State106"
  , v @"Msg106" /\ v @"State107"
  , v @"Msg107" /\ v @"State108"
  , v @"Msg108" /\ v @"State109"
  , v @"Msg109" /\ v @"State110"
  , v @"Msg110" /\ v @"State111"
  , v @"Msg111" /\ v @"State112"
  , v @"Msg112" /\ v @"State113"
  , v @"Msg113" /\ v @"State114"
  , v @"Msg114" /\ v @"State115"
  , v @"Msg115" /\ v @"State116"
  , v @"Msg116" /\ v @"State117"
  , v @"Msg117" /\ v @"State118"
  , v @"Msg118" /\ v @"State119"
  , v @"Msg119" /\ v @"State120"
  , v @"Msg120" /\ v @"State121"
  , v @"Msg121" /\ v @"State122"
  , v @"Msg122" /\ v @"State123"
  , v @"Msg123" /\ v @"State124"
  , v @"Msg124" /\ v @"State125"
  , v @"Msg125" /\ v @"State126"
  , v @"Msg126" /\ v @"State127"
  , v @"Msg127" /\ v @"State128"
  , v @"Msg128" /\ v @"State129"
  , v @"Msg129" /\ v @"State130"
  , v @"Msg130" /\ v @"State131"
  , v @"Msg131" /\ v @"State132"
  , v @"Msg132" /\ v @"State133"
  , v @"Msg133" /\ v @"State134"
  , v @"Msg134" /\ v @"State135"
  , v @"Msg135" /\ v @"State136"
  , v @"Msg136" /\ v @"State137"
  , v @"Msg137" /\ v @"State138"
  , v @"Msg138" /\ v @"State139"
  , v @"Msg139" /\ v @"State140"
  , v @"Msg140" /\ v @"State141"
  , v @"Msg141" /\ v @"State142"
  , v @"Msg142" /\ v @"State143"
  , v @"Msg143" /\ v @"State144"
  , v @"Msg144" /\ v @"State145"
  , v @"Msg145" /\ v @"State146"
  , v @"Msg146" /\ v @"State147"
  , v @"Msg147" /\ v @"State148"
  , v @"Msg148" /\ v @"State149"
  , v @"Msg149" /\ v @"State150"
  , v @"Msg150" /\ v @"State151"
  , v @"Msg151" /\ v @"State152"
  , v @"Msg152" /\ v @"State153"
  , v @"Msg153" /\ v @"State154"
  , v @"Msg154" /\ v @"State155"
  , v @"Msg155" /\ v @"State156"
  , v @"Msg156" /\ v @"State157"
  , v @"Msg157" /\ v @"State158"
  , v @"Msg158" /\ v @"State159"
  , v @"Msg159" /\ v @"State160"
  , v @"Msg160" /\ v @"State161"
  , v @"Msg161" /\ v @"State162"
  , v @"Msg162" /\ v @"State163"
  , v @"Msg163" /\ v @"State164"
  , v @"Msg164" /\ v @"State165"
  , v @"Msg165" /\ v @"State166"
  , v @"Msg166" /\ v @"State167"
  , v @"Msg167" /\ v @"State168"
  , v @"Msg168" /\ v @"State169"
  , v @"Msg169" /\ v @"State170"
  , v @"Msg170" /\ v @"State171"
  , v @"Msg171" /\ v @"State172"
  , v @"Msg172" /\ v @"State173"
  , v @"Msg173" /\ v @"State174"
  , v @"Msg174" /\ v @"State175"
  , v @"Msg175" /\ v @"State176"
  , v @"Msg176" /\ v @"State177"
  , v @"Msg177" /\ v @"State178"
  , v @"Msg178" /\ v @"State179"
  , v @"Msg179" /\ v @"State180"
  , v @"Msg180" /\ v @"State001"
  ]
