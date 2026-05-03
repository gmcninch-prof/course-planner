--
-- Time-stamp: <2026-05-03 Sun 13:32 EDT - george@valhalla>
--

import MLML.Codec
import MLML.Expression
import CoursePlanner.Calendar

open Calendar

namespace Semester

/-- A date or date range for an exception -/
inductive ExceptDate where
  | single (d  : String)
  | range  (dr : DateRange)
  deriving Repr

instance : Codec.Decode ExceptDate where
  decode expr := open Codec in
    (do let dr ← (Decode.decode expr : Except String DateRange)
        pure <| .range dr)  <|>
    (do let d ← (Decode.decode expr : Except String String)
        pure <| .single d)  <|>
    .error s!"Expected ExceptDate (Range constructor or string); got {repr expr}"

/-- A semester calendar exception -- holiday, admin note, alternate DOW, or no-class day -/
inductive Exception where
  | holiday  (descr : String) (date : ExceptDate)
  | admin    (descr : String) (date : ExceptDate)
  | altDow   (descr : String) (date : ExceptDate) (dow : DOW)
  | noClass  (descr : String) (date : ExceptDate)
  deriving Repr

instance : Codec.Decode Exception where
  decode
    | .Record "Holiday" fs => do
        let descr ← Codec.decodeField "descr" fs
        let date  ← Codec.decodeField "date" fs
        pure <| .holiday descr date
    | .Record "Admin" fs => do
        let descr     ← Codec.decodeField "descr" fs
        let date      ← Codec.decodeField "date" fs
        pure <| .admin descr date
    | .Record "AltDow" fs => do
        let descr ← Codec.decodeField "descr" fs
        let date  ← Codec.decodeField "date" fs
        let dow   ← Codec.decodeField "dow" fs
        pure <| .altDow descr date dow
    | .Record "NoClass" fs => do
        let descr ← Codec.decodeField "descr" fs
        let date  ← Codec.decodeField "date" fs
        pure <| .noClass descr date
    | e => .error s!"Expected Exception (Holiday, Admin, AltDow, NoClass); got {repr e}"

/-- Full specification of a semester, as parsed from a conf file -/
structure SemSpec where
  semester      : Semester
  semDates      : DateRange
  finalsDates   : DateRange
  rpDates       : DateRange
  exceptions    : List Exception
  deriving Repr

instance : Codec.Decode SemSpec where
  decode
    | .Record "Semester" fs => do
        let semester      ← Codec.decodeField "semester" fs
        let semDates      ← Codec.decodeField "semesterDates" fs
        let finalsDates   ← Codec.decodeField "finalsDates" fs
        let rpDates       ← Codec.decodeField "rpDates" fs
        let exceptions    ← Codec.decodeField "exceptions" fs
        pure { semester, semDates, finalsDates, rpDates, exceptions }
    | e => .error s!"Expected Semester; got {reprStr e}"

def lookupSemester (sems : List SemSpec) (sem : Semester) : Except String SemSpec :=
  match sems.find? (fun spec => spec.semester == sem) with
  | none => .error s!"Failed to find semester {reprStr sem}."
  | some spec => .ok spec

end Semester
