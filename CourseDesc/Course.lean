--
-- Time-stamp: <2026-04-21 Tue 16:35 EDT - george@valhalla>
--

import CourseDesc.Codec
import CourseDesc.Semester

namespace Course

open Semester

-- Course types

inductive ScheduleDetails where
  | DowTufts (dow : DOW) (time : EventTime) (location : String)
  | DowActual (dow : DOW) (time : EventTime) (location : String)
  | DowDue (dow : DOW) (deadline : String)
  | Date (date : String) (time : EventTime) (location : String)
  | DateDue (date : String) (deadline : String)
  deriving Repr

instance : Codec.Decode ScheduleDetails where
  decode 
    | .Constructor "DowTufts" fs => do
        let dow ← Codec.decodeField "dow" fs 
        let time ← Codec.decodeField "time" fs
        let location ← Codec.decodeField "location" fs
        pure <| .DowTufts dow time location
    | .Constructor "DowActual" fs => do
        let dow ← Codec.decodeField "dow" fs 
        let time ← Codec.decodeField "time" fs
        let location ← Codec.decodeField "location" fs
        pure <| .DowActual dow time location
    | .Constructor "DowDue" fs => do
        let dow ← Codec.decodeField "dow" fs 
        let deadline ← Codec.decodeField "deadline" fs
        pure <| .DowDue dow deadline
    | .Constructor "Date" fs => do
        let date ← Codec.decodeField "date" fs 
        let time ← Codec.decodeField "time" fs
        let location ← Codec.decodeField "location" fs
        pure <| .Date date time location
    | .Constructor "DateDue" fs => do
        let date ← Codec.decodeField "date" fs 
        let deadline ← Codec.decodeField "deadline" fs
        pure <| .DateDue date deadline        
    | e => .error s!"Expected SchedulesDetails; got {e}"
           

inductive CourseComponent where
  | Lecture    : (sched : List ScheduleDetails) 
               → (description : String) 
               → (topics : List String)
               → CourseComponent
  | Recitation : (sched : List ScheduleDetails) 
               → (description : String) 
               → (instructor : String) 
               → (topics : List String)
               → CourseComponent
  | Assignment : (sched : List ScheduleDetails) 
               → (description : String) 
               → (assignments : List String)
               → CourseComponent
  | Exam       : (sched : List ScheduleDetails) 
               → (description : String)
               → CourseComponent
  deriving Repr

instance : Codec.Decode CourseComponent where
  decode
    | .Constructor "Lecture" fs => do
       let sched ← Codec.decodeField "sched" fs
       let description ← Codec.decodeField "description" fs
       let topics ← Codec.decodeField "topics" fs
       pure <| .Lecture sched description topics
    | .Constructor "Recitation" fs => do
       let sched ← Codec.decodeField "sched" fs
       let description ← Codec.decodeField "description" fs
       let instructor ← Codec.decodeField "instructor" fs
       let topics ← Codec.decodeField "topics" fs
       pure <| .Recitation sched description instructor topics
    | .Constructor "Assignment" fs => do
       let sched ← Codec.decodeField "sched" fs
       let description ← Codec.decodeField "description" fs
       let assignments ← Codec.decodeField "assignments" fs
       pure <| .Assignment sched description assignments
    | .Constructor "Exam" fs => do
       let sched ← Codec.decodeField "sched" fs
       let description ← Codec.decodeField "description" fs
       pure <| .Exam sched description 
    | e => .error s!"Expected CourseComponent; got {e}"


def CourseComponent.sched : CourseComponent → List ScheduleDetails
  | .Lecture sched _ _ => sched
  | .Recitation sched _ _ _ => sched
  | .Assignment sched _ _ => sched
  | .Exam sched _ => sched


def CourseComponent.description : CourseComponent → String
  | .Lecture _ description _ => description
  | .Recitation _ description _ _ => description
  | .Assignment _ description _ => description
  | .Exam _ description => description

inductive Task where
  | Repeating (description : String) (dow : DOW) (taskStaffList : List String)
  | Single (description : String) (deadline : String) (taskStaff : String)
  | Meeting (description : String) (time : EventTime) (location : String) (dow : DOW)
  deriving Repr

instance : Codec.Decode Task where
  decode
    | .Constructor "Repeating" fs => do
       let description ← Codec.decodeField "description" fs
       let dow ← Codec.decodeField "dow" fs
       let taskStaffList ← Codec.decodeField "taskStaffList" fs
       pure <| .Repeating description dow taskStaffList
    | .Constructor "Single" fs => do
       let description ← Codec.decodeField "description" fs
       let deadline ← Codec.decodeField "deadline" fs
       let taskStaff ← Codec.decodeField "taskStaff" fs
       pure <| .Single description deadline taskStaff
    | .Constructor "Meeting" fs => do
       let description ← Codec.decodeField "description" fs
       let time ← Codec.decodeField "time" fs
       let location ← Codec.decodeField "location" fs
       let dow ← Codec.decodeField "dow" fs       
       pure <| .Meeting description time location dow
    | e => .error s!"Expected Task; received {e}"

def Task.description : Task → String
  | .Repeating desc _ _ => desc
  | .Single desc _ _ => desc
  | .Meeting desc _ _ _ => desc

def Task.dow : Task -> Option DOW
  | .Repeating _ dow _ => some dow
  | .Single _ _ _ => none 
  | .Meeting _ _ _ dow => some dow

structure TargetSpec where
  dir : String
  base : String
  org : String
  deriving Repr

instance : Codec.Decode TargetSpec where
  decode
    | .Constructor "Target" fs => do
      let dir ← Codec.decodeField "dir" fs
      let base ← Codec.decodeField "base" fs
      let org ← Codec.decodeField "org" fs
      pure <| { dir, base , org }
    | e => .error s!"Expected TargetSpec; received {e}"

structure Course where
  semester : Semester
  title : String
  sections : List String
  instructors : List String
  teachingAssts : List String
  target : TargetSpec
  description : String
  components : List CourseComponent
  tasks : List Task
  deriving Repr

instance : Codec.Decode Course where
  decode
    | .Constructor "Course" fs => do
       let semester ← Codec.decodeField "semester" fs
       let title ← Codec.decodeField "title" fs
       let sections ← Codec.decodeField "sections" fs
       let instructors ← Codec.decodeField "instructors" fs
       let teachingAssts ← Codec.decodeField "teachingAssts" fs
       let target ← Codec.decodeField "target" fs       
       let description ← Codec.decodeField "description" fs       
       let components ← Codec.decodeField "components" fs       
       let tasks ← Codec.decodeField "tasks" fs              
       pure <| { semester
               , title
               , sections
               , instructors
               , teachingAssts
               , target
               , description
               , components
               , tasks
               }
    | e => .error s!"Expected Course; received {e}"

end Course
