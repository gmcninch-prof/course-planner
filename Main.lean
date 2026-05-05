import MLML.Pipeline
import CoursePlanner
import Markdown

open Calendar
open Semester
open Course
open Pipeline
open Output

def loadSemesters (dir : String) : IO (List SemSpec) := do
  let entries ← System.FilePath.readDir dir
  let mlmlFiles := entries.toList.filter (fun e => e.fileName.endsWith ".mlml")
  let results ← mlmlFiles.mapM (fun e => do
    let text ← IO.FS.readFile e.path
    let result : Except String (List SemSpec) := parseAndDecode text
    match result with
    | .ok s => pure s
    | .error err => do
        IO.eprintln s!"Warning: failed to parse {e.fileName}: {err}"
        pure [])
  return results.flatten


def run (courseFile outputDir orgDir semesterDir : String) 
        (reports : List String) : IO Unit := do
  let courseText ← IO.FS.readFile courseFile
  let specs ← loadSemesters semesterDir
  match parseAndDecode courseText with
  | .error e => IO.println s!"Course parse error: {e}"
  | .ok course =>
      match courseCalendar course specs with
      | .error e => IO.println s!"Pipeline error: {e}"
      | .ok cc => do
          let writeIf (name : String) (content : String) (path : String) : IO Unit := do
            if reports.contains name || reports.contains "all" then
              IO.FS.writeFile path content
              IO.println s!"Wrote {path}"
          writeIf "grad"        (report cc gradEntries)         s!"{outputDir}/grad.md"
          writeIf "calendar"    (report cc allEntries)          s!"{outputDir}/calendar.md"
          writeIf "lectures"    (report cc lectureEntries)      s!"{outputDir}/lectures.md"
          writeIf "assignments" (report cc assignmentEntries)   s!"{outputDir}/assignments.md"
          let orgPath := s!"{orgDir}/{OrgOutput.orgFileName cc}"
          writeIf "org"         (OrgOutput.courseCalendarToOrg cc) orgPath

def main (args : List String) : IO Unit :=
  match args with
  | [courseFile, outputDir, orgDir, semesterDir] =>
      run courseFile outputDir orgDir semesterDir ["all"]
  | [courseFile, outputDir, orgDir, semesterDir, reports] =>
      run courseFile outputDir orgDir semesterDir (reports.splitOn ",")
  | _ => IO.println "Usage: course-planner <course-file> <output-dir> <org-dir> <semesterDir> [reports]"

