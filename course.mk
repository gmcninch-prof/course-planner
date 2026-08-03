# course.mk -- shared recipes for per-course/per-seminar/per-advisee
# Makefiles that drive course-planner.
#
# A directory's own Makefile should set at least COURSE_FILE, OUTPUT_DIR,
# MD_FILES, and REPORT_TYPES, then include this file, e.g.:
#
#   COURSE_FILE  = math245-fall26.mlml
#   OUTPUT_DIR   = calendar
#   MD_FILES     = $(OUTPUT_DIR)/calendar.md $(OUTPUT_DIR)/lectures.md $(OUTPUT_DIR)/assignments.md
#   REPORT_TYPES = calendar,lectures,assignments,org
#
#   include $(HOME)/.local/bin/course.mk
#
# `make install` in this repo copies course.mk to $(HOME)/.local/bin
# alongside the course-planner binary, so per-directory Makefiles include
# the installed copy rather than reaching into this source checkout.
# See course-makefile.sample for a copy-pasteable starting point.

PLANNER      ?= course-planner
SEMESTER_DIR ?= $(HOME)/prof-univ/semester-specs
ORG_DIR      ?= $(HOME)/org
REPORT_TYPES ?= all

$(OUTPUT_DIR):
	mkdir -p $(OUTPUT_DIR)

reports: $(OUTPUT_DIR)
	$(PLANNER) $(COURSE_FILE) $(OUTPUT_DIR) $(ORG_DIR) $(SEMESTER_DIR) "$(REPORT_TYPES)"
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
