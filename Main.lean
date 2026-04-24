import CoursePlanner.Pipeline
import CoursePlanner.Calendar
import CoursePlanner.Semester
import CoursePlanner.Course
import MLML.Pipeline

open Calendar
open Semester
open Course
open Pipeline

def main (args : List String) : IO Unit := do
  match args with
  | [courseFile, semFile] =>
      let courseText ← IO.FS.readFile courseFile
      let semText    ← IO.FS.readFile semFile
      match parseAndDecode courseText, parseAndDecode semText with
      | .ok course, .ok (spec : SemSpec) =>
          match courseCalendar course [spec] with
          | .ok days => IO.println s!"Got {days.length} days"
          | .error e => IO.println s!"Pipeline error: {e}"
      | .error e, _ => IO.println s!"Course parse error: {e}"
      | _, .error e => IO.println s!"Semester parse error: {e}"
  | _ => IO.println "Usage: course-planner <course-file> <semester-file>"


