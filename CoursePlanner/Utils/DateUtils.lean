--
-- Time-stamp: <2026-04-30 Thu 08:56 EDT - george@valhalla>
--

import Std.Time

namespace CoursePlanner.Utils

open Std.Time in
def prevMonday (d : PlainDate) : PlainDate :=
  let offset : Day.Offset := match d.weekday with
    | .monday    => 0
    | .tuesday   => 1
    | .wednesday => 2
    | .thursday  => 3
    | .friday    => 4
    | .saturday  => 5
    | .sunday    => 6
  d.subDays offset

open Std.Time in
def weekSince (base day : PlainDate) : Nat :=
  let startMon := prevMonday base
  let diff : Day.Offset := day.toDaysSinceUNIXEpoch - startMon.toDaysSinceUNIXEpoch
  1 + diff.val.toNat / 7

open Std.Time in
def dateRange (a b : PlainDate) : List PlainDate :=
  let n : Day.Offset := b.toDaysSinceUNIXEpoch - a.toDaysSinceUNIXEpoch
  (List.range (n.val.toNat + 1)).map (fun i => a.addDays <| .ofNat i)


end CoursePlanner.Utils
