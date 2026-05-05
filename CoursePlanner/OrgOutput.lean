import CoursePlanner.Calendar
import CoursePlanner.Pipeline
import CoursePlanner.Output

open Calendar
open Pipeline
open Output

namespace OrgOutput

def orgDir := "/home/george/org"

def formatOrgDate (date : Std.Time.PlainDate) : String :=
  let y := date.year
  let m := date.month.toNat
  let d := date.day.toNat
  let mm := if m < 10 then s!"0{m}" else s!"{m}"
  let dd := if d < 10 then s!"0{d}" else s!"{d}"
  s!"{y}-{mm}-{dd}"

def formatOrgTimestamp (date : Std.Time.PlainDate) (dow : DOW) (time : EventTime) : String :=
  let d := formatOrgDate date
  let w := formatDOW dow
  match time with
  | .timeRange start stop => s!"<{d} {w} {start}-{stop}>"
  | .pointInTime t        => s!"<{d} {w} {t}>"
  | .allDay               => s!"<{d} {w}>"

def entryToOrg (courseName : String) (day : AcademicDay) (entry : CalEntry) : Option String :=
  match entry with
  | .event time loc eventType desc _ seq _ =>
      let seqStr := match seq with | some n => s!"\n   {eventType} {n}" | none => s!"\n   {eventType}"
      let ts := formatOrgTimestamp day.date day.tuftsDow time
      some s!"** {courseName}: {desc}\n   {ts}\n   {loc}{seqStr}\n   {desc}"
  | .deadline time desc _ seq _ =>
      let seqStr := match seq with | some n => s!"Assignment {n}" | none => "Assignment"
      let ts := formatOrgTimestamp day.date day.tuftsDow time
      some s!"** {courseName}: {seqStr}\n   {ts}\n   {desc}"
  | .meeting desc time loc _ =>
      let ts := formatOrgTimestamp day.date day.tuftsDow time
      s!"** {courseName}: {desc} ({loc})\n   {ts}"
  | .task desc staff _ =>
      let ts := formatOrgTimestamp day.date day.tuftsDow .allDay
      s!"** {courseName}: {desc}\n   {ts}\n   {staff}"
  | .noClass _ => none
  | .admin _ => none
      
  -- | .noClass desc =>
  --     let ts := formatOrgTimestamp day.date day.tuftsDow .allDay
  --     s!"** {desc}\n   {ts}"
  -- | .admin desc =>
  --     let ts := formatOrgTimestamp day.date day.tuftsDow .allDay
  --     s!"** Univ: {desc}\n   {ts}"

def dayToOrg (courseName : String) (day : AcademicDay) : String :=
  day.entries.filterMap (entryToOrg courseName day) |> "\n".intercalate

def orgFileName (cc : CourseCalendar) : String :=
  s!"{cc.course.title}--{repr cc.course.semester}.org"

def courseCalendarToOrg (cc : CourseCalendar) : String :=
  let courseName := cc.course.title
  let header := s!"* {courseName}\n:PROPERTIES:\n:CATEGORY: {courseName}\n:END:\n"
  let body := cc.days.filterMap (fun day =>
      let s := dayToOrg courseName day
      if s.isEmpty then none else some s)
    |> "\n".intercalate
  header ++ "\n" ++ body

end OrgOutput
