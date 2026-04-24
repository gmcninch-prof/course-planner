

import Init.System.IO

import MLML.Tokens

import MLML.Parse

import CoursePlanner.Calendar
import MLML.Codec

def getTokens (file : String) : IO (List Token) := do
  let (s : String)  ← IO.FS.readFile file
  pure <| tokenize s


def getExpression (file : String) : IO (Except String Expression) := do
  let (s : String)  ← IO.FS.readFile file
  let toks : List Token := tokenize s
  pure <| Except.map Prod.fst <| parseExpression toks 

def printExpression (file : String) : IO Unit := do
  let expr ← getExpression file 
  match expr with
  | .ok expr => IO.println <| expr.render 0
  | .error e => IO.println e

open Codec in  
def getSemester (file : String) : IO (Except String (List Semester.SemSpec)) := do
  let (s : String)  ← IO.FS.readFile file
  let toks : List Token := tokenize s
  match Except.map Prod.fst <| parseExpression toks  with
  | .ok expr => pure <| Decode.decode expr
      --IO.println <| reprStr <| Decode.decode expr
  | .error e => .error e
  

open Codec in  
def getCourse (file : String) : IO (Except String Course.Course) := do
  let (s : String)  ← IO.FS.readFile file
  let toks : List Token := tokenize s
  match Except.map Prod.fst <| parseExpression toks  with
  | .ok expr => pure <| Decode.decode expr
      --IO.println <| reprStr <| Decode.decode expr
  | .error e => do
    IO.println "erroring here..."
    .error e

def math136 : String := "data/math136-spring26.mlml" 
def ay2025 : String := "data/AY2025-2026.mlml"

def main : IO Unit := do
  let result ← getCourse math136
  IO.println <| reprStr result

#eval getTokens math136
    
#eval getSemester ay2025

#eval getCourse math136

#eval printExpression ay2025


#eval printExpression math136
