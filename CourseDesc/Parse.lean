

import CourseDesc.Course
import CourseDesc.Tokens

def Parser (α : Type) := List Token → Except String (α × List Token)

-- def parseTask : Parser Task := do
--   let tag ← parseIdent
--   match tag with
--   | "Meeting" => Task.Meeting <$> parseRecord parseMeetingFields
--   | "Repeating" => Task.Repeating <$> parseRecord parseRepeatingFields
--   | other => throw s!"unknown Task constructor: {other}"
