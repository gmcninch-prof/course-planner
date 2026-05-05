--
-- Time-stamp: <2026-05-05 Tue 12:30 EDT - george@sortilege>
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
    | .Record "Mon" [] => .ok .monday
    | .Record "Tue" [] => .ok .tuesday
    | .Record "Wed" [] => .ok .wednesday
    | .Record "Thu" [] => .ok .thursday
    | .Record "Fri" [] => .ok .friday
    | .Record "Sat" [] => .ok .saturday
    | .Record "Sun" [] => .ok .sunday
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
    | .Record "Fall" []  => .ok .fall
    | .Record "Spring" [] => .ok .spring
    | e            => .error s!"Expected Term; got {repr e}"


/-- Date Range -/

structure DateRange where
  start : String
  stop  : String
  deriving Repr

instance : Codec.Decode DateRange where
  decode
    | .Record "Range" fs => do
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
    | .Record "Semester" fs => do
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
    | .Record "AllDay" [] => .ok .allDay
    | .Record "TimeRange" fields => do
        let start ← decodeField "start" fields
        let stop  ← decodeField "stop" fields
        return .timeRange start stop
    | .Record "PointInTime" fields => do
        let time ← decodeField "time" fields
        return .pointInTime time
    | e => .error s!"Expected EventTime; got {repr e}"

inductive EventType where
  | Lecture
  | Recitation
  | Exam
  | OfficeHour
  | GradMeeting
  deriving Repr, BEq
  
instance : ToString EventType where
  toString
    | .Lecture     => "Lecture"
    | .Recitation  => "Recitation"
    | .OfficeHour => "Office Hours"
    | .GradMeeting => "Grad Meeting"
    | .Exam        => "Exam"  

/-- A calendar entry attached to an academic day -/
inductive CalEntry where
  | event    (time : EventTime)
             (loc : String)
             (eventType : EventType)
             (description : String)
             (details : List String)
             (sequence : Option Nat)
             (courseName : Option String)
  | deadline (time : EventTime)
             (description : String)
             (details : List String)
             (sequence : Option Nat)
             (courseName : Option String)
  | noClass  (description : String)
  | admin    (description : String)
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

