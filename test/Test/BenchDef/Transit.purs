module Test.BenchDef.Transit where

import Prelude

import Data.Generic.Rep (class Generic)
import Transit (type (:*), type (:@), type (>|), Empty, Transit, match, mkUpdateGeneric, return)
import Type.Function (type ($))

-- x
data State
  = State01
  | State02
  | State03
  | State04
  | State05
  | State06
  | State07
  | State08
  | State09
  | State10
  | State11
  | State12
  | State13
  | State14
  | State15
  | State16
  | State17
  | State18
  | State19
  | State20
  | State21
  | State22
  | State23
  | State24
  | State25
  | State26
  | State27
  | State28
  | State29
  | State30
  | State31
  | State32
  | State33
  | State34
  | State35
  | State36
  | State37
  | State38
  | State39
  | State40
  | State41
  | State42
  | State43
  | State44
  | State45
  | State46
  | State47
  | State48
  | State49
  | State50

derive instance Generic State _

data Msg
  = Msg01
  | Msg02
  | Msg03
  | Msg04
  | Msg05
  | Msg06
  | Msg07
  | Msg08
  | Msg09
  | Msg10
  | Msg11
  | Msg12
  | Msg13
  | Msg14
  | Msg15
  | Msg16
  | Msg17
  | Msg18
  | Msg19
  | Msg20
  | Msg21
  | Msg22
  | Msg23
  | Msg24
  | Msg25
  | Msg26
  | Msg27
  | Msg28
  | Msg29
  | Msg30
  | Msg31
  | Msg32
  | Msg33
  | Msg34
  | Msg35
  | Msg36
  | Msg37
  | Msg38
  | Msg39
  | Msg40
  | Msg41
  | Msg42
  | Msg43
  | Msg44
  | Msg45
  | Msg46
  | Msg47
  | Msg48
  | Msg49
  | Msg50

derive instance Generic Msg _

type Size5Transit =
  Transit $ Empty
    :* ("State01" :@ "Msg01" >| "State02")
    :* ("State02" :@ "Msg02" >| "State03")
    :* ("State03" :@ "Msg03" >| "State04")
    :* ("State04" :@ "Msg04" >| "State05")
    :* ("State05" :@ "Msg05" >| "State06")
    :* ("State06" :@ "Msg06" >| "State07")
    :* ("State07" :@ "Msg07" >| "State08")
    :* ("State08" :@ "Msg08" >| "State09")
    :* ("State09" :@ "Msg09" >| "State10")
    :* ("State10" :@ "Msg10" >| "State11")
    :* ("State11" :@ "Msg11" >| "State12")
    :* ("State12" :@ "Msg12" >| "State13")
    :* ("State13" :@ "Msg13" >| "State14")
    :* ("State14" :@ "Msg14" >| "State15")
    :* ("State15" :@ "Msg15" >| "State16")
    :* ("State16" :@ "Msg16" >| "State17")
    :* ("State17" :@ "Msg17" >| "State18")
    :* ("State18" :@ "Msg18" >| "State19")
    :* ("State19" :@ "Msg19" >| "State20")
    :* ("State20" :@ "Msg20" >| "State21")
    :* ("State21" :@ "Msg21" >| "State22")
    :* ("State22" :@ "Msg22" >| "State23")
    :* ("State23" :@ "Msg23" >| "State24")
    :* ("State24" :@ "Msg24" >| "State25")
    :* ("State25" :@ "Msg25" >| "State26")
    :* ("State26" :@ "Msg26" >| "State27")
    :* ("State27" :@ "Msg27" >| "State28")
    :* ("State28" :@ "Msg28" >| "State29")
    :* ("State29" :@ "Msg29" >| "State30")
    :* ("State30" :@ "Msg30" >| "State31")
    :* ("State31" :@ "Msg31" >| "State32")
    :* ("State32" :@ "Msg32" >| "State33")
    :* ("State33" :@ "Msg33" >| "State34")
    :* ("State34" :@ "Msg34" >| "State35")
    :* ("State35" :@ "Msg35" >| "State36")
    :* ("State36" :@ "Msg36" >| "State37")
    :* ("State37" :@ "Msg37" >| "State38")
    :* ("State38" :@ "Msg38" >| "State39")
    :* ("State39" :@ "Msg39" >| "State40")
    :* ("State40" :@ "Msg40" >| "State41")
    :* ("State41" :@ "Msg41" >| "State42")
    :* ("State42" :@ "Msg42" >| "State43")
    :* ("State43" :@ "Msg43" >| "State44")
    :* ("State44" :@ "Msg44" >| "State45")
    :* ("State45" :@ "Msg45" >| "State46")
    :* ("State46" :@ "Msg46" >| "State47")
    :* ("State47" :@ "Msg47" >| "State48")
    :* ("State48" :@ "Msg48" >| "State49")
    :* ("State49" :@ "Msg49" >| "State50")
    :* ("State50" :@ "Msg50" >| "State01")

update :: State -> Msg -> State
update = mkUpdateGeneric @Size5Transit
  (match @"State01" @"Msg01" \_ _ -> return @"State02" unit)
  (match @"State02" @"Msg02" \_ _ -> return @"State03" unit)
  (match @"State03" @"Msg03" \_ _ -> return @"State04" unit)
  (match @"State04" @"Msg04" \_ _ -> return @"State05" unit)
  (match @"State05" @"Msg05" \_ _ -> return @"State06" unit)
  (match @"State06" @"Msg06" \_ _ -> return @"State07" unit)
  (match @"State07" @"Msg07" \_ _ -> return @"State08" unit)
  (match @"State08" @"Msg08" \_ _ -> return @"State09" unit)
  (match @"State09" @"Msg09" \_ _ -> return @"State10" unit)
  (match @"State10" @"Msg10" \_ _ -> return @"State11" unit)
  (match @"State11" @"Msg11" \_ _ -> return @"State12" unit)
  (match @"State12" @"Msg12" \_ _ -> return @"State13" unit)
  (match @"State13" @"Msg13" \_ _ -> return @"State14" unit)
  (match @"State14" @"Msg14" \_ _ -> return @"State15" unit)
  (match @"State15" @"Msg15" \_ _ -> return @"State16" unit)
  (match @"State16" @"Msg16" \_ _ -> return @"State17" unit)
  (match @"State17" @"Msg17" \_ _ -> return @"State18" unit)
  (match @"State18" @"Msg18" \_ _ -> return @"State19" unit)
  (match @"State19" @"Msg19" \_ _ -> return @"State20" unit)
  (match @"State20" @"Msg20" \_ _ -> return @"State21" unit)
  (match @"State21" @"Msg21" \_ _ -> return @"State22" unit)
  (match @"State22" @"Msg22" \_ _ -> return @"State23" unit)
  (match @"State23" @"Msg23" \_ _ -> return @"State24" unit)
  (match @"State24" @"Msg24" \_ _ -> return @"State25" unit)
  (match @"State25" @"Msg25" \_ _ -> return @"State26" unit)
  (match @"State26" @"Msg26" \_ _ -> return @"State27" unit)
  (match @"State27" @"Msg27" \_ _ -> return @"State28" unit)
  (match @"State28" @"Msg28" \_ _ -> return @"State29" unit)
  (match @"State29" @"Msg29" \_ _ -> return @"State30" unit)
  (match @"State30" @"Msg30" \_ _ -> return @"State31" unit)
  (match @"State31" @"Msg31" \_ _ -> return @"State32" unit)
  (match @"State32" @"Msg32" \_ _ -> return @"State33" unit)
  (match @"State33" @"Msg33" \_ _ -> return @"State34" unit)
  (match @"State34" @"Msg34" \_ _ -> return @"State35" unit)
  (match @"State35" @"Msg35" \_ _ -> return @"State36" unit)
  (match @"State36" @"Msg36" \_ _ -> return @"State37" unit)
  (match @"State37" @"Msg37" \_ _ -> return @"State38" unit)
  (match @"State38" @"Msg38" \_ _ -> return @"State39" unit)
  (match @"State39" @"Msg39" \_ _ -> return @"State40" unit)
  (match @"State40" @"Msg40" \_ _ -> return @"State41" unit)
  (match @"State41" @"Msg41" \_ _ -> return @"State42" unit)
  (match @"State42" @"Msg42" \_ _ -> return @"State43" unit)
  (match @"State43" @"Msg43" \_ _ -> return @"State44" unit)
  (match @"State44" @"Msg44" \_ _ -> return @"State45" unit)
  (match @"State45" @"Msg45" \_ _ -> return @"State46" unit)
  (match @"State46" @"Msg46" \_ _ -> return @"State47" unit)
  (match @"State47" @"Msg47" \_ _ -> return @"State48" unit)
  (match @"State48" @"Msg48" \_ _ -> return @"State49" unit)
  (match @"State49" @"Msg49" \_ _ -> return @"State50" unit)
  (match @"State50" @"Msg50" \_ _ -> return @"State01" unit)

updateClassic :: State -> Msg -> State
updateClassic state msg = case state, msg of
  State01, Msg01 -> State02
  State02, Msg02 -> State03
  State03, Msg03 -> State04
  State04, Msg04 -> State05
  State05, Msg05 -> State06
  State06, Msg06 -> State07
  State07, Msg07 -> State08
  State08, Msg08 -> State09
  State09, Msg09 -> State10
  State10, Msg10 -> State11
  State11, Msg11 -> State12
  State12, Msg12 -> State13
  State13, Msg13 -> State14
  State14, Msg14 -> State15
  State15, Msg15 -> State16
  State16, Msg16 -> State17
  State17, Msg17 -> State18
  State18, Msg18 -> State19
  State19, Msg19 -> State20
  State20, Msg20 -> State21
  State21, Msg21 -> State22
  State22, Msg22 -> State23
  State23, Msg23 -> State24
  State24, Msg24 -> State25
  State25, Msg25 -> State26
  State26, Msg26 -> State27
  State27, Msg27 -> State28
  State28, Msg28 -> State29
  State29, Msg29 -> State30
  State30, Msg30 -> State31
  State31, Msg31 -> State32
  State32, Msg32 -> State33
  State33, Msg33 -> State34
  State34, Msg34 -> State35
  State35, Msg35 -> State36
  State36, Msg36 -> State37
  State37, Msg37 -> State38
  State38, Msg38 -> State39
  State39, Msg39 -> State40
  State40, Msg40 -> State41
  State41, Msg41 -> State42
  State42, Msg42 -> State43
  State43, Msg43 -> State44
  State44, Msg44 -> State45
  State45, Msg45 -> State46
  State46, Msg46 -> State47
  State47, Msg47 -> State48
  State48, Msg48 -> State49
  State49, Msg49 -> State50
  State50, Msg50 -> State01
  _, _ -> state
