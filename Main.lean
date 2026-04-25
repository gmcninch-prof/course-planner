import CoursePlanner.Pipeline
import CoursePlanner.Calendar
import CoursePlanner.Semester
import CoursePlanner.Course
import MLML.Pipeline

open Calendar
open Semester
open Course
open Pipeline

def semester_file := "data/AY2025-26-Spring.mlml"
def course_file := "data/math136-spring26.mlml"

def main (args : List String) : IO Unit := do
  match args with
  | [courseFile, semFile] =>
      let courseText ← IO.FS.readFile courseFile
      let semText    ← IO.FS.readFile semFile
      match parseAndDecode courseText, parseAndDecode semText with
      | .ok course, .ok (spec : SemSpec) =>
          match courseCalendar course [spec] with
          | .ok days =>
            let withEntries := days.filter (fun d => !d.entries.isEmpty)
            withEntries.take 5 |>.forM (fun d =>
              IO.println s!"{repr d.date} ({repr d.tuftsDow}): {d.entries.length} entries")
          --| .ok days => IO.println s!"Got {days.length} days"
          -- | .ok days =>
          --   let totalEntries := days.foldl (fun n d => n + d.entries.length) 0
          --   IO.println s!"Got {days.length} days with {totalEntries} total entries"          
          | .error e => IO.println s!"Pipeline error: {e}"
      | .error e, _ => IO.println s!"Course parse error: {e}"
      | _, .error e => IO.println s!"Semester parse error: {e}"
  | _ => IO.println "Usage: course-planner <course-file> <semester-file>"




#eval main [course_file, semester_file]
