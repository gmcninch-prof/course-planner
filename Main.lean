import MLML.Pipeline
import CoursePlanner
import Markdown

open Calendar
open Semester
open Course
open Pipeline
open Output

def semesterDir := "/home/george/prof-teach-assets/semester-specs"


def loadSemesters (dir : String) : IO (List SemSpec) := do
  let entries ← System.FilePath.readDir dir
  let mlmlFiles := entries.toList.filter (fun e => e.fileName.endsWith ".mlml")
  let results ← mlmlFiles.mapM (fun e => do
    let text ← IO.FS.readFile e.path
    return (parseAndDecode text : Except String (List SemSpec)))
  return results.filterMap (fun r => match r with | .ok s => some s | .error _ => none)
    |>.flatten
  
def main (args : List String) : IO Unit :=
  match args with
  | [courseFile, outputDir] => do
      let courseText ← IO.FS.readFile courseFile
      let specs ← loadSemesters semesterDir
      IO.println s!"Loaded {specs.length} semester specs"
      specs.forM (fun s => IO.println s!"  {repr s.semester}")
      
      match parseAndDecode courseText with
      | .error e => IO.println s!"Course parse error: {e}"
      | .ok course =>
          IO.println s!"Looking for: {repr course.semester}"
          match courseCalendar course specs with
          | .error e => IO.println s!"Pipeline error: {e}"
          | .ok cc => do
              IO.FS.writeFile s!"{outputDir}/calendar.md" (fullCalendarReport cc)
              IO.FS.writeFile s!"{outputDir}/lectures.md" (lectureReport cc)
              IO.FS.writeFile s!"{outputDir}/assignments.md" (assignmentReport cc)
              IO.println s!"Written reports for {cc.course.title}"
              
              let orgPath := s!"{OrgOutput.orgDir}/{OrgOutput.orgFileName cc}"
              IO.FS.writeFile orgPath (OrgOutput.courseCalendarToOrg cc)
              IO.println s!"Written org file to {orgPath}"
              
  | _ => IO.println "Usage: course-planner <course-file> <output-dir>"
  
  
