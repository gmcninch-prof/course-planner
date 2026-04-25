import CoursePlanner.Calendar
import MLML
import MLML.Codec

--open MLML

def s := "TimeRange { stop = \"11\" start = \"10\" }"

def expr : Except String Expression := expressionFromString s

def x : Except String Calendar.EventTime := expr >>= Codec.Decode.decode 

#eval x
