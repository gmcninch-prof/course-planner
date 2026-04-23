--
-- Time-stamp: <2026-04-23 Thu 09:40 EDT - george@valhalla>
--

import MLML.Codec
import MLML.Expression


namespace Semester

inductive Term where
  | Fall | Spring
  deriving Repr 

instance : Codec.Decode Term where
  decode
    | .Id "Fall"   => .ok .Fall
    | .Id "Spring" => .ok .Spring
    | e            => .error s!"Expected Term, got {repr e}"
  
structure Semester where
  term : Term
  ay : Nat

instance : Codec.Decode Semester where
  decode 
    | .Constructor "Semester" fs => do
      let term     ← Codec.decodeField "term" fs
      let ay       ← Codec.decodeField "ay" fs
      pure { term, ay }
    | e => .error s!"Expected semester, got {repr e}"

instance : Repr Semester where
  reprPrec s _ := 
  let term := match s.term with
    | Term.Fall => "Fall"
    | Term.Spring => "Spring"
  s!"AY{reprStr s.ay}-{reprStr (s.ay + 1)}--{term}"

inductive DOW where
  | Mon | Tue | Wed | Thu | Fri | Sat | Sun
  deriving Repr

instance : Codec.Decode Semester.DOW where
  decode
    | .Id "Mon" => .ok .Mon
    | .Id "Tue" => .ok .Tue
    | .Id "Wed" => .ok .Wed
    | .Id "Thu" => .ok .Thu
    | .Id "Fri" => .ok .Fri
    | .Id "Sat" => .ok .Sat
    | .Id "Sun" => .ok .Sun
    | e => .error s!"expected DOW, got {repr e}"

structure EventTime where
  start : String
  stop : String
  deriving Repr

instance : Codec.Decode EventTime where
  decode
    | .Constructor "EventTime" fs => do
      let start   ← Codec.decodeField "start" fs
      let stop    ← Codec.decodeField "end" fs
      pure {start, stop}
    | e => .error s!"Expected EventTime, got {repr e}"

structure DateRange where
  start : String
  stop : String 
  deriving Repr

instance : Codec.Decode DateRange where
  decode
    | .Constructor "Range" fs => do
      let start   ← Codec.decodeField "start" fs
      let stop    ← Codec.decodeField "end" fs
      pure {start, stop}
    | e => .error s!"Expected EventTime, got {repr e}"

-- Semester types
inductive ExceptDate where
  | Date (d : String)
  | DateRange (dr : DateRange)
  deriving Repr

instance : Codec.Decode ExceptDate where
  decode expr := open Codec in
    (do let dr ← (Decode.decode expr : Except String DateRange); pure (.DateRange dr)) <|>
    (do let d  ← (Decode.decode expr : Except String String); pure (.Date d))    <|>
    .error s!"expected ExceptDate (Range constructor or string), got {repr expr}"        
        
inductive Exception where
  | EHoliday (descr : String) (date : ExceptDate)
  | EAdmin (adminType : String) (adminDescription : String) (date : ExceptDate)
  | EAltDow (descr : String) (date : ExceptDate) (dow : DOW)
  | ENoClass (descr : String) (date : ExceptDate)
  deriving Repr

instance : Codec.Decode Exception where
  decode 
    | .Constructor "Holiday" fs => do
      let descr ← Codec.decodeField "descr" fs
      let date ← Codec.decodeField "date" fs
      pure <| .EHoliday descr date
    | .Constructor "Admin" fs => do
      let adminType ← Codec.decodeField "type" fs
      let descr ← Codec.decodeField "descr" fs
      let date ← Codec.decodeField "date" fs
      pure <| .EAdmin adminType descr date
    | .Constructor "AltDow" fs => do
      let descr ← Codec.decodeField "descr" fs
      let date ← Codec.decodeField "date" fs
      let dow ← Codec.decodeField "dow" fs
      pure <| .EAltDow descr date dow
    | .Constructor "NoClass" fs => do
      let descr ← Codec.decodeField "descr" fs
      let date ← Codec.decodeField "date" fs
      pure <| .ENoClass descr date
    | e => .error s!"Expected Exception (Holiday, Admin, AltDow, NoClass); got {repr e}"

structure SemSpec where
  ay : String
  semId : String
  semesterDescr : String
  semDates : DateRange
  finalsDates : DateRange
  rpDates : DateRange
  exceptions : List Exception
  deriving Repr

instance : Codec.Decode SemSpec where
  decode
    | .Constructor "Semester" fs => do
      let ay ← Codec.decodeField "ay" fs
      let semId ← Codec.decodeField "semId" fs
      let semesterDescr ← Codec.decodeField "semesterDescr" fs
      let semDates ← Codec.decodeField "semesterDates" fs
      let finalsDates ← Codec.decodeField "finalsDates" fs
      let rpDates ← Codec.decodeField "rpDates" fs
      let exceptions ← Codec.decodeField "exceptions" fs
      pure { ay, semId, semesterDescr, semDates, finalsDates, rpDates, exceptions}
    | e => .error s!"Expected Semester; get {repr e}"

end Semester
