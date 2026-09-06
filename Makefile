LAKE_BINARY = course_planner
INSTALLED_NAME = course-planner

BUILD_DIR = .lake/build/bin
INSTALL_DIR = $(HOME)/.local/bin

OUTPUT_DIR = output

TEST_DATA         = examples/math136-spring26.mlml
TEST_OUTPUT       = examples
TEST_SEMESTER_DIR = examples/semester-specs
TEST_MD_FILES     = $(wildcard $(TEST_OUTPUT)*.md)
TEST_ORG_OUTPUT   = $(TEST_OUTPUT)


.PHONY: all build install clean reports test test-html test-pdf test-reports

all: build # test

build:
	lake build

install: build
	cp $(BUILD_DIR)/$(LAKE_BINARY) $(INSTALL_DIR)/$(INSTALLED_NAME)
	cp course.mk $(INSTALL_DIR)/course.mk

$(OUTPUT_DIR):
	mkdir -p $(OUTPUT_DIR)

# reports: $(OUTPUT_DIR)
# 	$(INSTALL_DIR)/$(BINARY) $(COURSE_FILE) $(OUTPUT_DIR)

clean:
	lake clean
	rm -f $(OUTPUT_DIR)/*.md
	rm -f $(TEST_OUTPUT)*.md
	rm -rf $(TEST_ORG_DIR)/*.org

update:
	lake update


#--------------------------------------------------------------------------------
# testing...

test:
	lake exe course_planner $(TEST_DATA) $(TEST_OUTPUT) $(TEST_ORG_OUTPUT) $(TEST_SEMESTER_DIR) 
	for f in $(TEST_OUTPUT)/*.md; do pandoc -f markdown-smart -t gfm -o $$f $$f; done

test-html: test
	for f in $(TEST_OUTPUT)/*.md; do \
	    pandoc -f markdown-smart -t html5 -o $${f%.md}.html $$f; \
	done

test-pdf: test
	for f in $(TEST_OUTPUT)/*.md; do \
	    pandoc -f markdown-smart -t pdf -o $${f%.md}.pdf $$f; \
	done
