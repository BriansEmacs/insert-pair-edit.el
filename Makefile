# ----------------------------------------------------------------------
#
# This file is a GNU Makefile used to run SDLC (Software Development
# LifeCycle) commands over the "Insert Pair Edit" (ipe) GNU Emacs
# package.
#
# This file is mainly just a wrapper to invoke Emacs in batch mode
# with some configuration variables.
#
# Most of the work is done by the Emacs Lisp file:
#
#   tools/ipe-build.el
#
# TARGETS
# * make cleanpkgs - Remove the build-packages directory.
# * make clean     - Remove the *.elc files.
# * make test      - Run the ipe ERT (Emacs Regression Tests).
# * make pkgs      - Download elint files from a package archive.
# * make lint      - Run elint over the ipe *.el files.
# * make linttest  - Run elint over the ipe test/*.el files.
# * make spell     - Run spell checker over the ipe *.el / *.txt files.
# * make build     - Byte compile the ipe *.el files.
# * make version   - Update ';; Version: X.X'  headers in ipe *.el files.
#
# CONFIGURATION
# You can change build configuration variables within a local
# 'config.mk' file.  Significant variable to set are:
#
# * EMACS_COMMAND - The path to the Emacs executable used to run the
#   batch build commands under `tools/ipe-build.el'.
# * TOP - The root directory for the `ipe' source code.
# * VERSION - The version set within the *.el files by the 'make
#   version' command.
# * PACKAGE_URL - The location from which to download build packages
#   (elint, multiple-cursors).
# * VERSION - A number (0-3) specifying how chatty to be when
#   building. 0 - quiet, 3 - debug.
#
# ----------------------------------------------------------------------

# ----------------------------------------------------------------------
## Settings
# ----------------------------------------------------------------------

TOP         := $(abspath $(dir $(lastword $(MAKEFILE_LIST))))
VERSION     := 1.0.0
PACKAGE_URL := https://melpa.org/packages/
VERBOSE     := 1

-include ./config.mk

# Users should usually prefer this over other *_CONFIG variables.
# We recommend that the value is set in the included "config.mk".
USER_CONFIG ?= "()"

ifdef EMACS_COMMAND
EMACS := $(EMACS_COMMAND)
else
EMACS ?= emacs
endif

BUILD_TOOLS    := tools/ipe-build.el
BUILD_PKGS_DIR := build-packages

ENV_CONFIG ?= "(progn\
  (setq ipe-base-dir \"$(TOP)\")\
  (setq ipe-build-pkgs-dir \"$(TOP)/$(BUILD_PKGS_DIR)\")\
  (setq ipe-package-url \"$(PACKAGE_URL)\")\
  (setq ipe-build-verbose \"$(VERBOSE)\"))"

LOAD_PATH ?= $(TOP)/$(BUILD_PKGS_DIR)

EVAL := $(EMACS) \
--no-site-file \
--batch \
$(addprefix -L ,$(LOAD_PATH)) \
--eval $(ENV_CONFIG) \
--eval $(USER_CONFIG) \
--load $(BUILD_TOOLS) \
--eval

# ----------------------------------------------------------------------
## TARGETS
# ----------------------------------------------------------------------

.PHONY: cleanpkgs pkgs clean test lint linttest spell build version
.FORCE:

all: pkgs clean test lint spell build

# ----------------------------------------------------------------------
### Clean build-package directory.
# ----------------------------------------------------------------------

cleanpkgs:
	@echo " • Cleaning build-packages..."
	@$(EVAL) "(ipe-build--clean-pkgs)"

# ----------------------------------------------------------------------
### Package Dependencies
# ----------------------------------------------------------------------

pkgs:
	@echo " • Downloading build-packages..."
	@$(EVAL) "(ipe-build--pkgs)"

# ----------------------------------------------------------------------
### Clean
# ----------------------------------------------------------------------

clean: 
	@echo " • Cleaning ipe package..."
	@$(EVAL) "(ipe-build--clean)"

# ----------------------------------------------------------------------
### Test
# ----------------------------------------------------------------------

test: clean
	@echo " • Testing ipe package..."
	@$(EVAL) "(ipe-build--test)"

# ----------------------------------------------------------------------
### Lint
# ----------------------------------------------------------------------

lint: pkgs
	@echo " • Linting ipe package..."
	@$(EVAL) "(ipe-build--lint)"

# ----------------------------------------------------------------------
### Lint test/*.el
# ----------------------------------------------------------------------

linttest: pkgs
	@echo " • Linting ipe tests..."
	@$(EVAL) "(ipe-build--linttest)"

# ----------------------------------------------------------------------
### Spell
# ----------------------------------------------------------------------

spell: 
	@echo " • Spell-checking ipe package..."
	@cat *.el *.txt *.md \
	  | aspell list --personal=./tools/ipe-dict-en.pws \
	  | sort --unique \
	  | while read word; do grep -on "\<$$word\>" *.el *.txt *.md; done \
	  | sort --field-separator=: --key=2 --numeric-sort

# ----------------------------------------------------------------------
### Build
# ----------------------------------------------------------------------

build: pkgs
	@echo " • Building ipe package..."
	@$(EVAL) "(ipe-build--build)"
	@makeinfo ipe.texi

# ----------------------------------------------------------------------
### Semver
# ----------------------------------------------------------------------

version: 
	@echo " • Versioning ipe package..."
	@$(EVAL) "(ipe-build--version \"$(VERSION)\")"

# ----------------------------------------------------------------------
# Local Variables:
# outline-regexp: "#\\(#+\\)"
# eval: (outline-minor-mode)
# End:
