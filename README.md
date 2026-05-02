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

For more information, see the output data and specs in `Test`.

## Per-course Makefile

Each course directory should have a `Makefile`; for example:

```makefile
COURSE_FILE = math136-spring26.mlml
OUTPUT_DIR = course-pages
PLANNER = course-planner                                # from ~/.local/bin/
SEMESTER_DIR = /home/george/prof-univ/semester-specs
ORG_DIR = /home/george/org

reports:
	$(PLANNER) $(COURSE_FILE) $(OUTPUT_DIR) $(ORG_DIR) $(SEMESTER_DIR)

# clean up the markdown table output	
	for f in $(MD_FILES); do pandoc -f markdown -t gfm -o $$f $$f; done

html: reports
	for f in $(MD_FILES); do \
		pandoc -f markdown -t html5 -o $${f%.md}.html $$f; \
	done

pdf: reports
	for f in $(MD_FILES); do \
		pandoc -f markdown -t pdf -o $${f%.md}.pdf $$f; \
	done

all: reports html pdf

.PHONY: reports html pdf all
```

## Dependencies

- [Lean 4](https://leanprover.github.io/)
- [MLML](https://github.com/gmcninch-prof/mlml) — configuration language and codec
- [Std4](https://github.com/leanprover/std4) — for `Std.Time` date handling
- [Markdown](https://github.com/predictable-machines/lean4-markdown)
  (or [here](https://github.com/gmcninch-prof/lean4-markdown) —
  Markdown generation library
- `pandoc` — for prettifying and converting output

