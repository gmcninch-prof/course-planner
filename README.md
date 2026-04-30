# course-planner

A Lean 4 tool for generating course calendar reports from structured course
and semester specifications written in `mlml`, a simple configuration language.

## Overview

`course-planner` takes a course specification file and a directory of semester
specification files, builds a structured academic calendar, and emits reports
in both Markdown and org-mode formats.

The pipeline:
1. Parse course and semester specs from `mlml` files
2. Build a skeleton calendar from semester date ranges
3. Apply university-wide exceptions (holidays, alternate schedules, admin dates)
4. Annotate calendar days with course events (lectures, exams, assignments, meetings)
5. Emit reports

## Usage

```bash
