-- Time-stamp: <2026-04-12 Sun 09:08 EDT - george@valhalla>
let Dow = < Mon | Tue | Wed | Thu | Fri | Sat | Sun >

let concat = https://prelude.dhall-lang.org/List/concat

let EventTime = { start : Text, end : Text }

let ScheduleDetails =
      < DowTufts : { dow : Dow, time : EventTime, location : Text }
      | DowActual : { dow : Dow, time : EventTime, location : Text }
      | DowDue : { dow : Dow, deadline : Text }
      | Date : { date : Text, time : EventTime, location : Text }
      | DateDue : { date : Text, deadline : Text }
      >

let CourseComponent =
      < Lecture :
          { sched : List ScheduleDetails
          , description : Text
          , topics : List Text
          }
      | Recitation :
          { sched : List ScheduleDetails
          , description : Text
          , instructor : Text
          , topics : List Text
          }
      | Assignment :
          { sched : List ScheduleDetails
          , description : Text
          , assignments : List Text
          }
      | Exam : { sched : List ScheduleDetails, description : Text }
      >

let Task =
      < Repeating : { description : Text, dow : Dow, taskStaffList : List Text }
      | Single : { description : Text, deadline : Text, taskStaff : Text }
      | Meeting :
          { description : Text, time : EventTime, location : Text, dow : Dow }
      >

let tasks =
      [ Task.Meeting
          { description = "Office Hours"
          , dow = Dow.Thu
          , time = { start = "15:00", end = "16:00" }
          , location = "JCC 587"
          }
      , Task.Meeting
          { description = "Office Hours"
          , dow = Dow.Wed
          , time = { start = "15:00", end = "16:00" }
          , location = "JCC 587"
          }
      ]

let homework =
      CourseComponent.Assignment
        { description = "Homework collected on gradescope"
        , sched =
          [ ScheduleDetails.DowDue { dow = Dow.Fri, deadline = "23:59" } ]
        , assignments = ./topics/assignments.dhall : List Text
        }

let lectures =
      CourseComponent.Lecture
        { sched =
          [ ScheduleDetails.DowTufts
              { dow = Dow.Mon
              , time = { start = "10:30", end = "11:45" }
              , location = "JCC 140"
              }
          , ScheduleDetails.DowTufts
              { dow = Dow.Wed
              , time = { start = "10:30", end = "11:45" }
              , location = "JCC 140"
              }
          ]
        , topics = ./topics/lectures.dhall : List Text
        , description = "course lecture"
        }

let midterm1 =
      CourseComponent.Exam
        { sched =
          [ ScheduleDetails.Date
              { date = "2026-02-18"
              , time = { start = "10:30", end = "11:45" }
              , location = "JCC 140"
              }
          ]
        , description = "midterm 1"
        }

let midterm2 =
      CourseComponent.Exam
        { sched =
          [ ScheduleDetails.Date
              { date = "2026-03-30"
              , time = { start = "10:30", end = "11:45" }
              , location = "JCC 140"
              }
          ]
        , description = "midterm 2"
        }

let final =
      CourseComponent.Exam
        { sched =
          [ ScheduleDetails.Date
              { date = "2026-05-01"
              , time = { start = "12:00", end = "14:00" }
              , location = "JCC 140"
              }
          ]
        , description = "final exam"
        }

in  [ { courseAY = "AY2025-2026"
      , courseSem = "spring"
      , title = "Math136"
      , sections = [ "01" ]
      , chair = "George McNinch"
      , instructors = [] : List Text
      , tas = [] : List Text
      , courseDescription = "Real Analysis II"
      , target =
        { dir = "course-pages", base = "Math136", org = "/home/george/org/" }
      , courseComponents = [ lectures, homework, midterm1, midterm2, final ]
      , courseTasks = tasks : List Task
      }
    ]
