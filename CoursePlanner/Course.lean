--
-- Time-stamp: <2026-05-05 Tue 17:26 EDT - george@valhalla>
--

import Std.Time
import MLML.Codec
import CoursePlanner.Calendar

open Calendar
open Std.Time

namespace Course

/-- The subset of `ScheduleDetails` patterns that recur every week (as opposed
    to `date`/`dateDue`, which each pick out a single specific day) -- and so
    are the only patterns it makes sense to thin to every-other-week via
    `ScheduleDetails.everyOtherWeek`. -/
inductive DowPattern where
  | tufts  (dow : DOW) (time : EventTime) (location : String)
  | actual (dow : DOW) (time : EventTime) (location : String)
  | due    (dow : DOW) (deadline : EventTime)
  deriving Repr

def DowPattern.dow : DowPattern → DOW
  | .tufts dow _ _  => dow
  | .actual dow _ _ => dow
  | .due dow _      => dow

instance : Codec.Decode DowPattern where
  decode
    | .Record "DowTufts" fs => do
        let dow      ← Codec.decodeField "dow" fs
        let time     ← Codec.decodeField "time" fs
        let location ← Codec.decodeField "location" fs
        pure <| .tufts dow time location
    | .Record "DowActual" fs => do
        let dow      ← Codec.decodeField "dow" fs
        let time     ← Codec.decodeField "time" fs
        let location ← Codec.decodeField "location" fs
        pure <| .actual dow time location
    | .Record "DowDue" fs => do
        let dow      ← Codec.decodeField "dow" fs
        let deadline ← Codec.decodeField "deadline" fs
        pure <| .due dow deadline
    | e => .error s!"Expected DowPattern; got {repr e}"

/-- Schedule pattern for a course component -/
inductive ScheduleDetails where
  | dowTufts  (dow : DOW) (time : EventTime) (location : String)
  | dowActual (dow : DOW) (time : EventTime) (location : String)
  | dowDue    (dow : DOW) (deadline : EventTime)
  | date      (date : String) (time : EventTime) (location : String)
  | dateDue   (date : String) (deadline : EventTime)
  /-- `inner` fires only in weeks with the same parity as the week containing
      `anchor` (the "YYYY-MM-DD" date of the first actual occurrence; it must
      fall on `inner`'s weekday, checked at decode time) -/
  | everyOtherWeek (anchor : String) (inner : DowPattern)
  deriving Repr

instance : Codec.Decode ScheduleDetails where
  decode
    | .Record "DowTufts" fs => do
        let dow      ← Codec.decodeField "dow" fs
        let time     ← Codec.decodeField "time" fs
        let location ← Codec.decodeField "location" fs
        pure <| .dowTufts dow time location
    | .Record "DowActual" fs => do
        let dow      ← Codec.decodeField "dow" fs
        let time     ← Codec.decodeField "time" fs
        let location ← Codec.decodeField "location" fs
        pure <| .dowActual dow time location
    | .Record "DowDue" fs => do
        let dow      ← Codec.decodeField "dow" fs
        let deadline ← Codec.decodeField "deadline" fs
        pure <| .dowDue dow deadline
    | .Record "Date" fs => do
        let date     ← Codec.decodeField "date" fs
        let time     ← Codec.decodeField "time" fs
        let location ← Codec.decodeField "location" fs
        pure <| .date date time location
    | .Record "DateDue" fs => do
        let date     ← Codec.decodeField "date" fs
        let deadline ← Codec.decodeField "deadline" fs
        pure <| .dateDue date deadline
    | .Record "EveryOtherWeek" fs => do
        let anchor ← Codec.decodeField "anchor" fs
        let inner  ← Codec.decodeField "inner" fs
        match PlainDate.parse anchor with
        | .error e => .error s!"everyOtherWeek: couldn't parse anchor date {anchor}: {e}"
        | .ok anchorDate =>
            if actualDow anchorDate != inner.dow then
              .error s!"everyOtherWeek: anchor {anchor} falls on {reprStr (actualDow anchorDate)}, but inner matches {reprStr inner.dow}"
            else
              pure <| .everyOtherWeek anchor inner
    | e => .error s!"Expected ScheduleDetails; got {repr e}"

inductive AppointmentType where
  | Lecture
  | Recitation (instructor : String)
  | OfficeHours
  | GradMeeting
  deriving Repr 

instance : Codec.Decode AppointmentType where
  decode
    | .Record "Lecture" _ => do
      pure <| .Lecture
    | .Record "Recitation" fs => do
      let instructor ← Codec.decodeField "Instructor" fs
      pure <| .Recitation instructor
    | .Record "OfficeHour" _ => do
      pure <| .OfficeHours
    | .Record "GradMeeting" _ => do
      pure <| .GradMeeting
    | e => .error s!"Expected AppointmentType; got {repr e}"

inductive CourseComponent where
  | appointment (sched       : List ScheduleDetails)
                (description : String)
                (topics      : List String)
                (kind        : AppointmentType)
  | assignment  (sched       : List ScheduleDetails)
                (description : String)
                (assignments : List String)
  | exam        (sched       : List ScheduleDetails)
                (description : String)
  /-- A recurring low-key activity that doesn't book its own room/time slot
      and doesn't suppress same-day lecture/appointment entries (unlike
      `exam`) -- e.g. a TA meeting, an exam-writing session, or a review
      activity folded into part of an existing lecture. -/
  | task        (sched       : List ScheduleDetails)
                (description : String)
  deriving Repr


instance : Codec.Decode CourseComponent where
  decode
    | .Record "Appointment" fs => do
        let kind        ← Codec.decodeField "kind" fs
        let sched       ← Codec.decodeField "sched" fs
        let description ← Codec.decodeField "description" fs
        let topics      ← Codec.decodeFieldList "topics" fs
        pure <| .appointment sched description topics kind
    | .Record "Assignment" fs => do
        let sched       ← Codec.decodeField "sched" fs
        let description ← Codec.decodeField "description" fs
        let assignments ← Codec.decodeField "assignments" fs
        pure <| .assignment sched description assignments
    | .Record "Exam" fs => do
        let sched       ← Codec.decodeField "sched" fs
        let description ← Codec.decodeField "description" fs
        pure <| .exam sched description
    | .Record "Task" fs => do
        let sched       ← Codec.decodeField "sched" fs
        let description ← Codec.decodeField "description" fs
        pure <| .task sched description
    | e => .error s!"Expected CourseComponent; got {repr e}"

/-- Extract the schedule from a CourseComponent, if it has one -/
def CourseComponent.sched : CourseComponent → List ScheduleDetails
  | .appointment sched _ _ _ => sched
  | .assignment sched _ _    => sched
  | .exam sched _            => sched
  | .task sched _            => sched

/-- Extract the description from a CourseComponent -/
def CourseComponent.description : CourseComponent → String
  | .appointment _ d _ _ => d
  | .assignment _ d _    => d
  | .exam _ d            => d
  | .task _ d            => d

structure Course where
  semester      : Semester
  title         : String
  sections      : List String
  instructors   : List String
  teachingAssts : List String
  description   : String
  components    : List CourseComponent
  deriving Repr

instance : Codec.Decode Course where
  decode
    | .Record "Course" fs => do
        let semester      ← Codec.decodeField "semester" fs
        let title         ← Codec.decodeField "title" fs
        let sections      ← Codec.decodeFieldList "sections" fs
        let instructors   ← Codec.decodeField "instructors" fs
        let teachingAssts ← Codec.decodeFieldList "teachingAssts" fs
        let description   ← Codec.decodeField "description" fs
        let components    ← Codec.decodeFieldList "components" fs
        pure <| { semester
                , title
                , sections
                , instructors
                , teachingAssts
                , description
                , components
                }
    | e => .error s!"Expected Course; got {repr e}"

def CourseComponent.needsSequence : CourseComponent → Bool
  | .appointment _ _ _ .Lecture 
  | .appointment _ _ _ (.Recitation _)  
  | .appointment _ _ _ .GradMeeting     => true
  | .assignment _ _ _    => true
  | _                    => false
  
def CourseComponent.topicForSeq (comp : CourseComponent) (seq : Nat) : Option String :=
  match comp with
  | .appointment _ _ topics _         => topics[seq - 1]?
  | .assignment _ _ assignments => assignments[seq - 1]?
  | _ => none

end Course
