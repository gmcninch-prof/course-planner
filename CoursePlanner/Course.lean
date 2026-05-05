--
-- Time-stamp: <2026-05-05 Tue 11:31 EDT - george@sortilege>
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
    | e => .error s!"Expected CourseComponent; got {repr e}" 

/-- Extract the schedule from a CourseComponent, if it has one -/
def CourseComponent.sched : CourseComponent → List ScheduleDetails
  | .appointment sched _ _ _ => sched
  | .assignment sched _ _    => sched
  | .exam sched _            => sched

/-- Extract the description from a CourseComponent -/
def CourseComponent.description : CourseComponent → String
  | .appointment _ d _ _ => d
  | .assignment _ d _    => d
  | .exam _ d            => d

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
