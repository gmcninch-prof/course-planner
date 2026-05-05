--
-- Time-stamp: <2026-05-05 Tue 12:28 EDT - george@sortilege>
--

import Std.Time
import CoursePlanner.Calendar
import CoursePlanner.Course
import CoursePlanner.Semester
import CoursePlanner.Utils.DateUtils
import Markdown

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
  | .admin descr date =>
      if matchDate date day then
        addEntry (.admin descr) day
      else day
  | .altDow descr date dow =>
      if matchDate date day then
        { addEntry (.admin descr) day with tuftsDow := dow }
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


def sdTime : ScheduleDetails → EventTime
  | .dowTufts _ time _  => time
  | .dowActual _ time _ => time
  | .dowDue _ deadline  => deadline
  | .date _ time _      => time
  | .dateDue _ deadline => deadline

def sdLoc : ScheduleDetails → String
  | .dowTufts _ _ loc  => loc
  | .dowActual _ _ loc => loc
  | .dowDue _ _        => ""
  | .date _ _ loc      => loc
  | .dateDue _ _       => ""


def makeEntry (comp : CourseComponent)
              (sd : ScheduleDetails)
              (desc : String)
              (seq : Option Nat)
              (courseName : Option String) : CalEntry :=
  match comp with
  | .appointment _ _ _ .Lecture        => .event (sdTime sd) (sdLoc sd) .Lecture desc [] seq courseName
  | .appointment _ _ _ .OfficeHours    => .event (sdTime sd) (sdLoc sd) .OfficeHour desc [] seq courseName  
  | .appointment _ _ _ .GradMeeting    => .event (sdTime sd) (sdLoc sd) .GradMeeting desc [] seq courseName  
  | .appointment _ _ _ (.Recitation _) => .event (sdTime sd) (sdLoc sd) .Recitation desc [] seq courseName  
  | .assignment _ _ _                  => .deadline (sdTime sd) desc [] seq courseName
  | .exam _ _                          => .event (sdTime sd) (sdLoc sd) .Exam desc [] seq courseName
  
  
inductive ComponentAction where
  | skip     -- don't fire, don't increment sequence
  | suppress -- increment sequence but don't add entry
  | fire     -- add entry and increment
  
def applyComponent (comp : CourseComponent) (courseName : Option String) (days : List AcademicDay) : List AcademicDay :=
  let scheds := comp.sched
  let (_, days') := days.foldl
    (fun (acc : Nat × List AcademicDay) day =>
      let (seq, processed) := acc
      match action comp day with
      | .skip     => (seq, processed ++ [day])
      | .suppress => (seq + 1, processed ++ [day])
      | .fire     =>
          let fired := scheds.any (matchesSchedule · day)
          if fired then
            let seqOpt := if comp.needsSequence then some seq else none
            let desc := match comp.topicForSeq seq with
              | some topic => topic
              | none       => comp.description
            let entry := scheds.findSome? (fun sd =>
              if matchesSchedule sd day then some (makeEntry comp sd desc seqOpt courseName)
              else none)
            match entry with
            | some e => (seq + 1, processed ++ [addEntry e day])
            | none   => (seq, processed ++ [day])
          else (seq, processed ++ [day]))
    (1, [])
  days'
where
  action : CourseComponent → AcademicDay → ComponentAction
    | .appointment _ _ _ _ => fun day =>
      let hasExam := day.entries.any (fun e => match e with | .event _ _ .Exam _ _ _ _ => true | _ => false)
      if !day.univOpen || day.status != .inTerm then .skip
      else if hasExam then .suppress
      else .fire
    | .assignment _ _ _  => fun day =>
      if !day.univOpen || day.status != .inTerm then .skip 
      else .fire
    | _ => fun day =>
      if !day.univOpen then .skip 
      else .fire  
  
def addCourseEntries (course : Course) (days : List AcademicDay) : List AcademicDay :=
  let exams := course.components.filter (fun c => match c with | .exam _ _ => true | _ => false)
  let others := course.components.filter (fun c => match c with | .exam _ _ => false | _ => true)
  let days := exams.foldl (fun days comp => applyComponent comp (some course.title) days) days
  others.foldl (fun days comp => applyComponent comp (some course.title) days) days  

structure CourseCalendar where
  course : Course
  days   : List AcademicDay   

def courseCalendar (course : Course) (specs : List SemSpec) : Except String CourseCalendar := do
  let semId := course.semester
  let spec ← match specs.find? (fun s => s.semester == semId) with
    | some s => .ok s
    | none   => .error s!"No semester spec found for {repr semId}"
  let days ← semesterDates spec
  let days := addCourseEntries course (applyExceptions spec.exceptions days)
  return { course, days }
    
  
end Pipeline

