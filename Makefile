BINARY = course-planner
INSTALLED_NAME = course-planner

BUILD_DIR = .lake/build/bin
INSTALL_DIR = $(HOME)/.local/bin

OUTPUT_DIR = output

.PHONY: all build install clean reports

all: build

build:
	lake build

install: build
	cp $(BUILD_DIR)/$(BINARY) $(INSTALL_DIR)/$(INSTALLED_NAME)

$(OUTPUT_DIR):
	mkdir -p $(OUTPUT_DIR)

# reports: $(OUTPUT_DIR)
# 	$(INSTALL_DIR)/$(BINARY) $(COURSE_FILE) $(OUTPUT_DIR)

clean:
	lake clean
	rm -f $(OUTPUT_DIR)/*.md

update:
	lake update
