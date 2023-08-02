# A handful of building niceties


PROJECT_NAME := fountain2latex

EXE_EXT :=

ifneq ($(USERPROFILE),)
	EXE_EXT := .exe
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

CABAL_FILE := $(PROJECT_NAME).cabal

COMPILED_FILENAME := $(PROJECT_NAME)$(EXE_EXT)
COMPILED_DIR := out
COMPILED_TARGET := $(COMPILED_DIR)/$(COMPILED_FILENAME)

SOURCES := src/Main.hs $(GIT_VERSION_MODULE)


.PHONY: install vartest vermode clean deepclean build help all
.SILENT: vartest help $(GIT_VERSION_MODULE) $(COMPILED_TARGET)
.ONESHELL: vartest

help:
	echo Type 'make build' to compile and leave the executable in ./out/
	echo Type 'make install' to compile and install the executable in your system

vartest:
	echo $$\(CABAL_FILE\)                  : $(CABAL_FILE)
	echo $$\(COMPILED_DIR\)                : $(COMPILED_DIR)
	echo $$\(COMPILED_FILENAME\)           : $(COMPILED_FILENAME)
	echo $$\(COMPILED_TARGET\)             : $(COMPILED_TARGET)
	echo $$\(COMMIT\)                      : $(COMMIT)
	echo $$\(GIT_VERSION_MODULE\)          : $(GIT_VERSION_MODULE)
	echo $$\(SOURCES\)                     : $(SOURCES)
	echo $$\(USERPROFILE\)                 : $(USERPROFILE)
	echo $$\(VERSION\)                     : $(VERSION)
	echo $$\(VERSION_COMMIT\)              : $(VERSION_COMMIT)
	echo $$\(VERSION_COMMIT_FILE\)         : $(VERSION_COMMIT_FILE)
	echo $$\(VERSION_COMMIT_FILE_CONTENTS\): $(VERSION_COMMIT_FILE_CONTENTS)

$(GIT_VERSION_MODULE): $(VERSION_COMMIT_FILE)
	echo module GitVersion where > $(GIT_VERSION_MODULE)
	echo >> $(GIT_VERSION_MODULE)
	echo gitVersion :: String >> $(GIT_VERSION_MODULE)
	echo gitVersion = \"$(VERSION_COMMIT)\" >> $(GIT_VERSION_MODULE)

$(COMPILED_TARGET): $(SOURCES) $(CABAL_FILE)
	cabal build -O2 --disable-debug-info --disable-shared --enable-executable-stripping -j $(CABAL_FLAGS)
	mkdir -p $(COMPILED_DIR)
	echo find dist-newstyle -type f -name $(COMPILED_FILENAME) -exec cp \{\} $(COMPILED_TARGET) \;
	find dist-newstyle -name $(COMPILED_FILENAME) -exec cp \{\} $(COMPILED_TARGET) \;

build: $(COMPILED_TARGET)

install: $(SOURCE) $(CABAL_FILE)
	cabal install -O2 --disable-debug-info --disable-shared --enable-executable-stripping -j --overwrite-policy=always $(CABAL_FLAGS)

README.md: README.tex
	./make-markdown-readme.sh

README.pdf: README.tex
	pdflatex README.tex

all: build README.md README.pdf

clean:
	-find . \( -iname \*.sw\? -o -iname \*\~ -o -iname \*\# \) -delete
	-rm *.{aux,out,log}
	-rm -rf $(GIT_VERSION_MODULE) $(VERSION_COMMIT_FILE)

deepclean: clean
	-rm -rf $(COMPILED_DIR) dist-newstyle
	-rm *.pdf

