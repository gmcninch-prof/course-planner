--
-- Time-stamp: <2026-05-05 Tue 22:07 EDT - george@valhalla>
--

import CoursePlanner.Calendar
import CoursePlanner.Pipeline
import Markdown

open Calendar
open Markdown

namespace Output

-- Date formatting

def formatDate (d : Std.Time.PlainDate) : String :=
  let m := d.month.toNat
  let day := d.day.toNat
  let mm := if m < 10 then s!"0{m}" else s!"{m}"
  let dd := if day < 10 then s!"0{day}" else s!"{day}"
  s!"{mm}/{dd}"

def formatDOW : DOW → String
  | .monday    => "Mon"
  | .tuesday   => "Tue"
  | .wednesday => "Wed"
  | .thursday  => "Thu"
  | .friday    => "Fri"
  | .saturday  => "Sat"
  | .sunday    => "Sun"

def formatEventTime : EventTime → String
  | .timeRange start stop => s!"{start}-{stop}"
  | .pointInTime t        => t
  | .allDay               => ""

-- Entry type label and details

def entryLabel : CalEntry → String
  | .event _ _ eventType _ _ (some n) _ => s!"{eventType} {n}"
  | .event _ _ eventType _ _ none _     => toString eventType
  | .deadline _ _ _ (some n) _          => s!"Assignment {n}"
  | .deadline _ _ _ none _              => "Assignment"
  | .noClass _                          => "Univ"
  | .admin _                            => "Univ"
  | .meeting _ _ _ _                    => "Meeting"
  | .task _ _ _                         => "Task"

def entryDetails : CalEntry → String
  | .event time loc _ desc _ _ _        => s!"{formatEventTime time} {loc} - {desc}"
  | .deadline time desc _ _ _           => s!"{formatEventTime time} {desc}"
  | .noClass desc                       => s!"**No classes** *{desc}*"
  | .admin desc                         => s!"*{desc}*"
  | .meeting desc time loc _            => s!"{formatEventTime time} {loc} - {desc}"
  | .task desc staff _                  => s!"{desc} ({staff})"

-- Row construction

def entryToRow (day : AcademicDay) (entry : CalEntry) : Vector TableCell 5 :=
  #v[ textCell (formatDate day.date)
    , textCell (formatDOW day.tuftsDow)
    , textCell (entryLabel entry)
    , textCell (toString day.week)
    , textCell (entryDetails entry)
    ]

-- Flatten days into (day, entry) pairs, preserving order

def dayRows (day : AcademicDay) : List (Vector TableCell 5) :=
  day.entries.map (entryToRow day)

-- The full calendar table

def calendarTable (filter : CalEntry → Bool) (cc : Pipeline.CourseCalendar) : List MarkdownTag :=
  let headers : Vector String 5 := #v["Date", "DOW", "Desc", "Week", "Details"]
  let rows := cc.days.flatMap (fun day =>
    { day with entries := day.entries.filter filter } |> dayRows)
  [{ element := .table { headers, rows } }]

-- def calendarTable (cc : Pipeline.CourseCalendar) : List MarkdownTag :=
--   let headers : Vector String 5 := #v["Date", "DOW", "Desc", "Week", "Details"]
--   let rows := cc.days.flatMap dayRows
--   [{ element := .table { headers, rows } }]

-- Top level report

def allEntries : CalEntry → Bool := fun _ => true

def lectureEntries : CalEntry → Bool
  | .event _ _ .Lecture _ _ _ _   => true
  | .event _ _ .Exam _ _ _ _      => true
  | .noClass _                    => true
  | .admin _                      => false
  | _                             => false

def gradEntries : CalEntry → Bool
  | .event _ _ .GradMeeting _ _ _ _      => true
  | .event _ _ .Exam _ _ _ _      => true  
  | .noClass _                    => true
  | _                             => false

def assignmentEntries : CalEntry → Bool
  | .deadline _ _ _ _ _ => true
  | _                   => false

-- def fullCalendarReport (cc : Pipeline.CourseCalendar) : String :=
--   let title := s!"# {cc.course.title} - {cc.course.description} - {repr cc.course.semester}\n\n"
--   title ++ renderMarkdown (calendarTable allEntries cc)

-- def lectureReport (cc : Pipeline.CourseCalendar) : String :=
--   let title := s!"# {cc.course.title} - Lectures - {repr cc.course.semester}\n\n"
--   title ++ renderMarkdown (calendarTable lectureEntries cc)

-- def assignmentReport (cc : Pipeline.CourseCalendar) : String :=
--   let title := s!"# {cc.course.title} - Assignments - {repr cc.course.semester}\n\n"
--   title ++ renderMarkdown (calendarTable assignmentEntries cc)

def report (cc : Pipeline.CourseCalendar) (title: String) (filter : CalEntry -> Bool) : String :=
  let title := s!"# {cc.course.title} - {title} - {repr cc.course.semester}\n\n"
  title ++ renderMarkdown (calendarTable filter cc)

end Output


