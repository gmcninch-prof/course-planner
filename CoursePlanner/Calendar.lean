--
-- Time-stamp: <2026-04-23 Thu 09:20 EDT - george@valhalla>
--

import CoursePlanner.Course

open Semester

inductive DayStatus where
  | InTerm | InFinals
  deriving Repr, BEq

inductive DayProperty where
  | Event    : (time : EventTime) 
             → (loc : String) 
             → (eventType : String)
             → (description : String) 
             → (details : List String)
             → (sequence : Option Nat) 
             → (courseName : Option String)
             → DayProperty
  | Deadline : (deadline : String) 
             → (dlType : String) 
             → (description : String)
             → (details : List String) 
             → (sequence : Option Nat)
             → DayProperty
  | NoClass  : (description : String) 
             → DayProperty
  | Admin    : (adminType : String) 
             → (description : String)
             → DayProperty
  | Meeting  : (description : String) 
             → (time : EventTime)
             → (location : String) 
             → (courseName : Option String)
             → DayProperty
  | Task     : (taskDescription : String) 
             → (taskStaff : String)
             → (courseName : Option String)
             → DayProperty
  deriving Repr

structure CalDay where
  caldate : String   -- or a proper Date type if you have one
  tuftsDOW : DOW
  univOpen : Bool
  status : DayStatus
  properties : List DayProperty
  week : Nat
  deriving Repr

