# A handful of building niceties


PROJECT_NAME := fountain2latex

EXE_EXT :=
CABAL_FLAGS := --enable-shared

ifneq ($(USERPROFILE),)
	EXE_EXT := .exe
	CABAL_FLAGS := --disable-shared
endif

# The executable reports its version, and we build a module for that.
VERSION := $(shell git tag -l | grep -E ^v\[0-9\] | head -n 1)
COMMIT := $(shell git log -n 1 | sed -re '/^commit/{s/^commit\s+([0-9a-fA-F]{10}).*/\1/p};d')
VERSION_COMMIT := $(VERSION)\#$(COMMIT)
VERSION_COMMIT_FILE := .version_commit
VERSION_COMMIT_FILE_CONTENTS := $(shell cat $(VERSION_COMMIT_FILE) 2> /dev/null || echo '')
GIT_VERSION_MODULE := src/GitVersion.hs
ifneq ($(VERSION_COMMIT),$(VERSION_COMMIT_FILE_CONTENTS))
	_ := $(shell echo "$(VERSION_COMMIT)" > $(VERSION_COMMIT_FILE))
endif

COMPILED_FILENAME := $(PROJECT_NAME)$(EXE_EXT)
COMPILED_DIR := out
COMPILED_TARGET := $(COMPILED_DIR)/$(COMPILED_FILENAME)

SOURCES := src/Main.hs $(GIT_VERSION_MODULE)


.PHONY: install vartest vermode clean
.SILENT: vartest clean
.ONESHELL: vartest clean

vartest:
	echo COMPILED_FILE               : $(COMPILED_FILE)
	echo USERPROFILE                 : $(USERPROFILE)
	echo VERSION_COMMIT              : $(VERSION_COMMIT)
	echo VERSION_COMMIT_FILE_CONTENTS: $(VERSION_COMMIT_FILE_CONTENTS)

$(GIT_VERSION_MODULE): $(VERSION_COMMIT_FILE)
	@echo module GitVersion where > $(GIT_VERSION_MODULE)
	@echo >> $(GIT_VERSION_MODULE)
	@echo gitVersion :: String >> $(GIT_VERSION_MODULE)
	@echo gitVersion = \"$(VERSION_COMMIT)\" >> $(GIT_VERSION_MODULE)

$(COMPILED_TARGET): $(SOURCES)
	cabal build -O2 --disable-debug-info --enable-executable-stripping -j $(CABAL_FLAGS)
	@mkdir -p $(COMPILED_DIR)
	@find dist-newstyle -name $(COMPILED_FILE) -exec cp {} $(COMPILED_TARGET)

clean:
	-rm $(GIT_VERSION_MODULE)
	-rm -rf $(COMPILED_DIR)

