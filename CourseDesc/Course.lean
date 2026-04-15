
namespace Semester

inductive Term where
  | Fall | Spring
  deriving Repr 
  
structure Semester where
  term : Term
  ay : Nat

instance : Repr Semester where
  reprPrec s _ := 
  let term := match s.term with
    | Term.Fall => "Fall"
    | Term.Spring => "Spring"
  s!"AY{reprStr s.ay}-{reprStr (s.ay + 1)}--{term}"

#eval (Semester.mk Term.Fall 2025)

-- Basic types
inductive DOW where
  | Mon | Tue | Wed | Thu | Fri | Sat | Sun
  deriving Repr

structure EventTime where
  start : String
  stop : String
  deriving Repr

-- Semester types
inductive ExceptDate where
  | Date (d : String)
  | Daterange (start : String) (stop : String)
  deriving Repr

inductive Exception where
  | EHoliday (descr : String) (date : ExceptDate)
  | EAdmin (adminType : String) (adminDescription : String) (date : ExceptDate)
  | EAltDow (descr : String) (date : ExceptDate) (dow : DOW)
  | ENoClass (descr : String) (date : ExceptDate)
  deriving Repr

structure SemSpec where
  ay : String
  semId : String
  semesterDescr : String
  semStart : String
  semEnd : String
  finalsStart : String
  finalsEnd : String
  readingPeriodStart : String
  readingPeriodEnd : String
  exceptions : List Exception
  deriving Repr

end Semester

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
