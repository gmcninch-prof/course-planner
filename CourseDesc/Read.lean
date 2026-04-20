import Init.System.IO

import CourseDesc.Tokens

import CourseDesc.Parse

import CourseDesc.Calendar
import CourseDesc.Codec

def get (file : String) : IO (Except String Expression) := do
  let (s : String)  ← IO.FS.readFile file
  let toks : List Token := tokenize s
  pure <| Except.map Prod.fst <| parseExpression toks.length toks 

open Codec in  
def getSemester (file : String) : IO (Except String (List Semester.SemSpec)) := do
  let (s : String)  ← IO.FS.readFile file
  let toks : List Token := tokenize s
  match Except.map Prod.fst <| parseExpression toks.length toks  with
  | .ok expr => pure <| Decode.decode expr
      --IO.println <| reprStr <| Decode.decode expr
  | .error e => .error e
  
  
 
    
#eval getSemester "AY2025-2026.conf"
--#eval getSemester "test-2.conf"
