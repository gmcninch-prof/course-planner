import CourseDesc.Course

open Semester

inductive SemStatus where
  | InTerm | InFinals
  deriving Repr, BEq

inductive DayProperty where
  | PEvent    : (time : EventTime) 
              → (loc : String) 
              → (eventType : String)
              → (description : String) 
              → (details : List String)
              → (sequence : Option Nat) 
              → (courseName : Option String)
              → DayProperty
  | PDeadline : (deadline : String) 
              → (dlType : String) 
              → (description : String)
              → (details : List String) 
              → (sequence : Option Nat)
              → DayProperty
  | PNoClass  : (description : String) 
              → DayProperty
  | PAdmin    : (adminType : String) 
              → (description : String)
              → DayProperty
  | PMeeting  : (description : String) 
              → (time : EventTime)
              → (location : String) 
              → (courseName : Option String)
              → DayProperty
  | PTask     : (taskDescription : String) 
              → (taskStaff : String)
              → (courseName : Option String)
              → DayProperty
  deriving Repr

structure CalDay where
  caldate : String   -- or a proper Date type if you have one
  tuftsDOW : DOW
  univOpen : Bool
  status : SemStatus
  properties : List DayProperty
  week : Nat
  deriving Repr
