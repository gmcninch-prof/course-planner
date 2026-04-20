--
-- Time-stamp: <2026-04-20 Mon 15:13 EDT - george@valhalla>
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

structure Course where
  ay : String
  semester : String
  title : String
  sections : List String
  chair : String
  instructors : List String
  tas : List String
  target : TargetSpec
  courseDescription : String
  courseComponents : List CourseComponent
  courseTasks : List Task
  deriving Repr

end Course
