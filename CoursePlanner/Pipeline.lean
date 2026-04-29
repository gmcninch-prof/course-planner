--
-- Time-stamp: <2026-04-29 Wed 16:08 EDT - george@sortilege>
--

import Std.Time
import CoursePlanner.Calendar
import CoursePlanner.Course
import CoursePlanner.Semester
import CoursePlanner.Utils.DateUtils

open Calendar
open Course
open Semester
open CoursePlanner.Utils
open Std.Time

namespace Pipeline

/-- Convert a parsed DateRange to a list of PlainDates -/
def expandDateRange (dr : Calendar.DateRange) : Except String (List PlainDate) := do
  let a ← PlainDate.parse dr.start
  let b ← PlainDate.parse dr.stop
  return dateRange a b

/-- Build a skeleton AcademicDay from a PlainDate and SemPhase -/
def makeDay (base : PlainDate) (status : SemPhase) (date : PlainDate) : AcademicDay :=
  { date     := date
  , tuftsDow := actualDow date         
  , univOpen := true
  , status   := status
  , entries  := []
  , week     := weekSince base date
  }

/-- Build the skeleton list of AcademicDays from a SemSpec -/
def semesterDates (spec : SemSpec) : Except String (List AcademicDay) := do
  let base    ← PlainDate.parse spec.semDates.start
  let term    ← expandDateRange spec.semDates
  let rp      ← expandDateRange spec.rpDates
  let finals  ← expandDateRange spec.finalsDates
  return (term.map (makeDay base .inTerm)   ++
          rp.map    (makeDay base .readingPeriod) ++
          finals.map (makeDay base .inFinals))


def matchDate (ed : Semester.ExceptDate) (day : AcademicDay) : Bool :=
  match ed with
  | .single d =>
      match PlainDate.parse d with
      | .ok date => day.date == date
      | .error _ => false
  | .range dr =>
      match PlainDate.parse dr.start, PlainDate.parse dr.stop with
      | .ok a, .ok b => (dateRange a b).contains day.date
      | _, _         => false

def addEntry (entry : CalEntry) (day : AcademicDay) : AcademicDay :=
  { day with entries := entry :: day.entries }

def applyException (exc : Semester.Exception) (day : AcademicDay) : AcademicDay :=
  match exc with
  | .holiday descr date =>
      if matchDate date day then
        { addEntry (.noClass descr) day with univOpen := false }
      else day
  | .noClass descr date =>
      if matchDate date day then
        { addEntry (.noClass descr) day with univOpen := false }
      else day
  | .admin adminType descr date =>
      if matchDate date day then
        addEntry (.admin adminType descr) day
      else day
  | .altDow descr date dow =>
      if matchDate date day then
        { addEntry (.admin "Tufts" descr) day with tuftsDow := dow }
      else day

def applyExceptions (exceptions : List Semester.Exception) (days : List AcademicDay) : List AcademicDay :=
  exceptions.foldl (fun days exc => days.map (applyException exc)) days

def matchesSchedule (sd : ScheduleDetails) (day : AcademicDay) : Bool :=
  match sd with
  | .dowTufts dow _ _  => day.tuftsDow == dow
  | .dowActual dow _ _ => actualDow day.date == dow
  | .dowDue dow _      => day.tuftsDow == dow
  | .date d _ _        =>
      match PlainDate.parse d with
      | .ok date => day.date == date
      | .error _ => false
  | .dateDue d _       =>
      match PlainDate.parse d with
      | .ok date => day.date == date
      | .error _ => false

def makeEntry (sd : ScheduleDetails) 
              (description : String) 
              (seq : Option Nat)
              (courseName : Option String) : CalEntry :=
  match sd with
  | .dowTufts _ time loc  => .event time loc "lecture" description [] seq courseName
  | .dowActual _ time loc => .event time loc "lecture" description [] seq courseName
  | .date _ time loc      => .event time loc "lecture" description [] seq courseName
  | .dowDue _ deadline    => .deadline deadline "" description [] seq courseName
  | .dateDue _ deadline   => .deadline deadline "" description [] seq courseName

  
def applyComponent (comp : CourseComponent) (days : List AcademicDay) : List AcademicDay :=
  let courseName := none
  let scheds := comp.sched
  let desc := comp.description
  let (_, days') := days.foldl (fun (acc : Nat × List AcademicDay) day =>
    let (seq, processed) := acc
    if !day.univOpen then
      (seq, processed ++ [day])  -- skip closed days, don't increment sequence
    else
      let fired := scheds.any (matchesSchedule · day)
      if fired then
        let seqOpt := if comp.needsSequence then some seq else none
        let entry := scheds.findSome? (fun sd =>
          if matchesSchedule sd day then some (makeEntry sd desc seqOpt courseName)
          else none)
        match entry with
        | some e => (seq + 1, processed ++ [addEntry e day])
        | none   => (seq, processed ++ [day])
      else (seq, processed ++ [day])) (1, [])
  days'  
  
-- def applyComponent (comp : CourseComponent) (days : List AcademicDay) : List AcademicDay :=
--   let courseName := none  -- we'll come back to this
--   let scheds := comp.sched
--   let desc := comp.description
--   let (_, days') := days.foldl (fun (acc : Nat × List AcademicDay) day =>
--     let (seq, processed) := acc
--     let fired := scheds.any (matchesSchedule · day)
--     if fired then
--       let seqOpt := if comp.needsSequence then some seq else none
--       let entry := scheds.findSome? (fun sd =>
--         if matchesSchedule sd day then some (makeEntry sd desc seqOpt courseName)
--         else none)
--       match entry with
--       | some e => (seq + 1, processed ++ [addEntry e day])
--       | none   => (seq, processed ++ [day])
--     else (seq, processed ++ [day])) (1, [])
--   days'


def addCourseEntries (course : Course) (days : List AcademicDay) : List AcademicDay :=
  course.components.foldl (fun days comp => applyComponent comp days) days
   
def courseCalendar (course : Course) (specs : List SemSpec) : Except String (List AcademicDay) := do
  let semId := course.semester
  let spec ← lookupSemester specs semId
  let days ← semesterDates spec
  return addCourseEntries course (applyExceptions spec.exceptions days)   

end Pipeline

