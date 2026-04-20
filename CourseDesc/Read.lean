import Init.System.IO

import CourseDesc.Tokens

def get (file : String) : IO (List Token) := do
  let s ← IO.FS.readFile file
  pure <| tokenize s
 
  
#eval get "AY2025-2026.conf"
