import CourseDesc.Course
import CourseDesc.Calendar

import CourseDesc.Expression

class Decode (α : Type) where
  decode : Expression → Except String α

def decodeField [Decode α] (name : String) (fs : List Field) : Except String α :=
  lookupField name fs >>= Decode.decode

instance [Decode α] : Decode (List α) where
  decode
    | .EList es => es.mapM Decode.decode
    | e => .error s!"expected list, got {repr e}"

instance : Decode String where
  decode
    | .StrLit s => .ok s
    | .Id s => .ok s
    | e => .error s!"expected raw string or id, got {repr e}"
    
instance : Decode Nat where
  decode
    | .NatLit n => .ok n
    | e => .error s!"expected raw Nat, got {repr e}"

instance : Decode Bool where
  decode
    | .BoolLit b => .ok b
    | e => .error s!"expected raw Nat, got {repr e}"

instance : Decode Semester.DOW where
  decode
    | .Id "Mon" => .ok .Mon
    | .Id "Tue" => .ok .Tue
    | .Id "Wed" => .ok .Wed
    | .Id "Thu" => .ok .Thu
    | .Id "Fri" => .ok .Fri
    | .Id "Sat" => .ok .Sat
    | .Id "Sun" => .ok .Sun
    | e => .error s!"expected DOW, got {repr e}"

instance : Decode SemStatus where
  decode
    | .Id "InTerm" => .ok .InTerm
    | .Id "InFinals" => .ok .InFinals
    | e => .error s!"expected SemStatus, got {repr e}"

instance : Decode DayProperty where
  decode :=
    sorry

instance : Decode CalDay where
  decode 
    | .Constructor "CalDay" fs => do
      let caldate    ← decodeField "caldate" fs
      let tuftsDOW   ← decodeField "tuftsDOW" fs
      let univOpen   ← decodeField "univOpen" fs
      let status     ← decodeField "SemStatus" fs
      let properties ← decodeField "properties" fs
      let week       ← decodeField "week" fs
      sorry
      pure { caldate
           , tuftsDOW
           , univOpen
           , status
           , properties
           , week
           }
    | e => .error s!"expected CalDay, got {repr e}"
