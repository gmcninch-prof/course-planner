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
course-planner <conf-file.mlml> <output-dir> <org-output-dir> <semester-dir>
```

Semester spec files are read from `<semester-dir>`

Markdown reports written to `<output-dir>`:
- `calendar.md` — full course calendar
- `lectures.md` — lectures and exams only
- `assignments.md` — assignment deadlines only

Org reports written to `<org-output-dir>`:
- `<Course>--<AY>--<Term>.org` — emacs org-mode calendar info

For more information, see the `Makefile` `test` target, the specs in
`examples/` and the resulting output data (markdown and org) in `examples/`.

## Per-course Makefile

Each course/seminar/advisee directory should have a thin `Makefile` that sets
a few variables and includes the shared recipes in
[`course.mk`](./course.mk). A ready-to-copy template is provided at
[`course-makefile.sample`](./course-makefile.sample) in this repo. Copy it to
your directory, rename it `Makefile`, and adjust the variables at the top to
match your setup.

The sample looks like this:

```makefile
COURSE_FILE  = math136-spring26.mlml
OUTPUT_DIR   = course-pages
MD_FILES     = $(OUTPUT_DIR)/calendar.md $(OUTPUT_DIR)/lectures.md $(OUTPUT_DIR)/assignments.md
REPORT_TYPES = calendar,lectures,assignments,org

# SEMESTER_DIR and ORG_DIR default to $(HOME)/prof-univ/semester-specs and
# $(HOME)/org respectively (see course.mk); override here if needed.

include $(HOME)/.local/bin/course.mk
```

`REPORT_TYPES` is the comma-separated list passed as the tool's final
argument (see `Main.lean`); it should match whatever `MD_FILES` names plus
`org` if you want the org-mode calendar written. `course.mk` provides the
`reports`, `html`, `pdf`, and `all` targets, along with the `pandoc`
post-processing steps — a per-directory Makefile shouldn't need to redefine
any of that.

`make install` copies `course.mk` to `$(HOME)/.local/bin` alongside the
`course-planner` binary, so per-directory Makefiles include the installed
copy rather than reaching into this source checkout.

## Dependencies

- [Lean 4](https://leanprover.github.io/)
- [MLML](https://github.com/gmcninch-prof/mlml) — configuration language and codec
- [Std4](https://github.com/leanprover/std4) — for `Std.Time` date handling
- [Markdown](https://github.com/predictable-machines/lean4-markdown)
  (or [here](https://github.com/gmcninch-prof/lean4-markdown) —
  Markdown generation library
- `pandoc` — for prettifying and converting output

