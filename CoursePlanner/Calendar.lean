--
-- Time-stamp: <2026-04-25 Sat 09:14 EDT - george@valhalla>
--

import Std.Time
import MLML.Codec

namespace Calendar

/-- Day of week -/
inductive DOW where
  | monday | tuesday | wednesday | thursday | friday | saturday | sunday
  deriving Repr, BEq

open Std.Time in
def DOW.ofWeekday : Weekday → DOW
  | .monday    => .monday
  | .tuesday   => .tuesday
  | .wednesday => .wednesday
  | .thursday  => .thursday
  | .friday    => .friday
  | .saturday  => .saturday
  | .sunday    => .sunday

open Std.Time in
def actualDow (d : PlainDate) : DOW :=
  DOW.ofWeekday d.weekday

instance : Codec.Decode DOW where
  decode
    | .Id "Mon" => .ok .monday
    | .Id "Tue" => .ok .tuesday
    | .Id "Wed" => .ok .wednesday
    | .Id "Thu" => .ok .thursday
    | .Id "Fri" => .ok .friday
    | .Id "Sat" => .ok .saturday
    | .Id "Sun" => .ok .sunday
    | e => .error s!"Expected DOW; got {repr e}"

/-- Phase of the academic semester -/
inductive SemPhase where
  | inTerm | readingPeriod | inFinals
  deriving Repr, BEq

/-- Academic term -- fall or spring -/
inductive Term where
  | fall | spring
  deriving Repr, BEq

instance : Codec.Decode Term where
  decode
    | .Id "Fall"   => .ok .fall
    | .Id "Spring" => .ok .spring
    | e            => .error s!"Expected Term; got {repr e}"


/-- Date Range -/

structure DateRange where
  start : String
  stop  : String
  deriving Repr

instance : Codec.Decode DateRange where
  decode
    | .Constructor "Range" fs => do
        let start ← Codec.decodeField "start" fs
        let stop  ← Codec.decodeField "stop" fs
        pure { start, stop }
    | e => .error s!"Expected DateRange; got {repr e}"

/-- Academic year and term, e.g. Fall 2025 -/
structure Semester where
  term : Term
  ay   : Nat
  deriving BEq

instance : Codec.Decode Semester where
  decode
    | .Constructor "Semester" fs => do
        let term ← Codec.decodeField "term" fs
        let ay   ← Codec.decodeField "ay" fs
        pure { term, ay }
    | e => .error s!"Expected Semester; got {repr e}"

instance : Repr Semester where
  reprPrec s _ :=
    let term := match s.term with
      | .fall   => "Fall"
      | .spring => "Spring"
    s!"AY{reprStr s.ay}-{reprStr (s.ay + 1)}--{term}"

/-- Time of an event -- a range, a point in time, or all-day -/
inductive EventTime where
  | timeRange   (start : String) (stop : String)
  | pointInTime (time : String)
  | allDay
  deriving Repr, BEq

open Codec in
instance : Codec.Decode EventTime where
  decode
    | .Id "AllDay" => .ok .allDay
    | .Constructor "TimeRange" fields => do
        let start ← decodeField "start" fields
        let stop  ← decodeField "stop" fields
        return .timeRange start stop
    | .Constructor "PointInTime" fields => do
        let time ← decodeField "time" fields
        return .pointInTime time
    | e => .error s!"Expected EventTime; got {repr e}"

/-- A calendar entry attached to an academic day -/
inductive CalEntry where
  | event    (time : EventTime)
             (loc : String)
             (eventType : String)
             (description : String)
             (details : List String)
             (sequence : Option Nat)
             (courseName : Option String)
  | deadline (time : EventTime)
             (dlType : String)
             (description : String)
             (details : List String)
             (sequence : Option Nat)
             (courseName : Option String)
  | noClass  (description : String)
  | admin    (adminType : String)
             (description : String)
  | meeting  (description : String)
             (time : EventTime)
             (location : String)
             (courseName : Option String)
  | task     (taskDescription : String)
             (taskStaff : String)
             (courseName : Option String)
  deriving Repr

/-- A single day in the academic calendar -/
structure AcademicDay where
  date     : Std.Time.PlainDate
  tuftsDow : DOW
  univOpen : Bool
  status   : SemPhase
  entries  : List CalEntry
  week     : Nat
  deriving Repr

end Calendar

