--
-- Time-stamp: <2026-04-30 Thu 10:46 EDT - george@valhalla>
--

--
-- Time-stamp: <2026-04-24 Fri 15:40 EDT - george@sortilege>
--

import MLML.Codec
import CoursePlanner.Calendar

open Calendar

namespace Course

/-- Schedule pattern for a course component -/
inductive ScheduleDetails where
  | dowTufts  (dow : DOW) (time : EventTime) (location : String)
  | dowActual (dow : DOW) (time : EventTime) (location : String)
  | dowDue    (dow : DOW) (deadline : EventTime)
  | date      (date : String) (time : EventTime) (location : String)
  | dateDue   (date : String) (deadline : EventTime)
  deriving Repr

instance : Codec.Decode ScheduleDetails where
  decode
    | .Constructor "DowTufts" fs => do
        let dow      ← Codec.decodeField "dow" fs
        let time     ← Codec.decodeField "time" fs
        let location ← Codec.decodeField "location" fs
        pure <| .dowTufts dow time location
    | .Constructor "DowActual" fs => do
        let dow      ← Codec.decodeField "dow" fs
        let time     ← Codec.decodeField "time" fs
        let location ← Codec.decodeField "location" fs
        pure <| .dowActual dow time location
    | .Constructor "DowDue" fs => do
        let dow      ← Codec.decodeField "dow" fs
        let deadline ← Codec.decodeField "deadline" fs
        pure <| .dowDue dow deadline
    | .Constructor "Date" fs => do
        let date     ← Codec.decodeField "date" fs
        let time     ← Codec.decodeField "time" fs
        let location ← Codec.decodeField "location" fs
        pure <| .date date time location
    | .Constructor "DateDue" fs => do
        let date     ← Codec.decodeField "date" fs
        let deadline ← Codec.decodeField "deadline" fs
        pure <| .dateDue date deadline
    | e => .error s!"Expected ScheduleDetails; got {repr e}"

/-- A component of a course -- lecture, recitation, assignment, exam, task, or meeting -/
inductive CourseComponent where
  | lecture    (sched : List ScheduleDetails)
               (description : String)
               (topics : List String)
  | recitation (sched : List ScheduleDetails)
               (description : String)
               (instructor : String)
               (topics : List String)
  | assignment (sched : List ScheduleDetails)
               (description : String)
               (assignments : List String)
  | exam       (sched : List ScheduleDetails)
               (description : String)
  | repeating  (description : String)
               (dow : DOW)
               (taskStaffList : List String)
  | single     (description : String)
               (deadline : EventTime)
               (taskStaff : String)
  | meeting    (description : String)
               (time : EventTime)
               (location : String)
               (dow : DOW)
  deriving Repr

instance : Codec.Decode CourseComponent where
  decode
    | .Constructor "Lecture" fs => do
        let sched       ← Codec.decodeField "sched" fs
        let description ← Codec.decodeField "description" fs
        let topics      ← Codec.decodeField "topics" fs
        pure <| .lecture sched description topics
    | .Constructor "Recitation" fs => do
        let sched       ← Codec.decodeField "sched" fs
        let description ← Codec.decodeField "description" fs
        let instructor  ← Codec.decodeField "instructor" fs
        let topics      ← Codec.decodeField "topics" fs
        pure <| .recitation sched description instructor topics
    | .Constructor "Assignment" fs => do
        let sched       ← Codec.decodeField "sched" fs
        let description ← Codec.decodeField "description" fs
        let assignments ← Codec.decodeField "assignments" fs
        pure <| .assignment sched description assignments
    | .Constructor "Exam" fs => do
        let sched       ← Codec.decodeField "sched" fs
        let description ← Codec.decodeField "description" fs
        pure <| .exam sched description
    | .Constructor "Repeating" fs => do
        let description   ← Codec.decodeField "description" fs
        let dow           ← Codec.decodeField "dow" fs
        let taskStaffList ← Codec.decodeField "taskStaffList" fs
        pure <| .repeating description dow taskStaffList
    | .Constructor "Single" fs => do
        let description ← Codec.decodeField "description" fs
        let deadline    ← Codec.decodeField "deadline" fs
        let taskStaff   ← Codec.decodeField "taskStaff" fs
        pure <| .single description deadline taskStaff
    | .Constructor "Meeting" fs => do
        let description ← Codec.decodeField "description" fs
        let time        ← Codec.decodeField "time" fs
        let location    ← Codec.decodeField "location" fs
        let dow         ← Codec.decodeField "dow" fs
        pure <| .meeting description time location dow
    | e => .error s!"Expected CourseComponent; got {repr e}"

/-- Extract the schedule from a CourseComponent, if it has one -/
def CourseComponent.sched : CourseComponent → List ScheduleDetails
  | .lecture sched _ _       => sched
  | .recitation sched _ _ _  => sched
  | .assignment sched _ _    => sched
  | .exam sched _            => sched
  | .repeating _ dow _       => [.dowDue dow .allDay]
  | .single _ deadline _     => [.dateDue "" deadline]
  | .meeting _ _ _ dow       => [.dowActual dow .allDay ""]

/-- Extract the description from a CourseComponent -/
def CourseComponent.description : CourseComponent → String
  | .lecture _ d _      => d
  | .recitation _ d _ _ => d
  | .assignment _ d _   => d
  | .exam _ d           => d
  | .repeating d _ _    => d
  | .single d _ _       => d
  | .meeting d _ _ _    => d

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
    | .Constructor "Course" fs => do
        let semester      ← Codec.decodeField "semester" fs
        let title         ← Codec.decodeField "title" fs
        let sections      ← Codec.decodeField "sections" fs
        let instructors   ← Codec.decodeField "instructors" fs
        let teachingAssts ← Codec.decodeField "teachingAssts" fs
        let description   ← Codec.decodeField "description" fs
        let components    ← Codec.decodeField "components" fs
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
  | .lecture _ _ _      => true
  | .recitation _ _ _ _ => true
  | .assignment _ _ _   => true
  | _                   => false
  

def CourseComponent.topicForSeq (comp : CourseComponent) (seq : Nat) : Option String :=
  match comp with
  | .lecture _ _ topics         => topics[seq - 1]?
  | .recitation _ _ _ topics    => topics[seq - 1]?
  | .assignment _ _ assignments => assignments[seq - 1]?
  | _ => none


end Course
