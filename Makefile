# The plutus-pab commands, contracts and hoogle environment
# are made availible by the nix shell defined in shell.nix.
# In most cases you should execute Make after entering nix-shell.

SHELL := /usr/bin/env bash

.PHONY: hoogle build test watch ghci readme_contents \
	format lint refactor requires_nix_shell

usage:
	@echo "usage: make <command> [OPTIONS]"
	@echo
	@echo "Available options:"
	@echo "  FLAGS   -- Additional options passed to --ghc-options"
	@echo
	@echo "Available commands:"
	@echo "  hoogle              -- Start local hoogle"
	@echo "  build               -- Run cabal v2-build"
	@echo "  test                -- Run cabal v2-test"
	@echo "  costing             -- Run cost-estimation benchmark"
	@echo "  coverage            -- Generate a coverage report of the tests"
	@echo "  ghci                -- Run stack ghci"
	@echo "  format              -- Apply source code formatting with fourmolu"
	@echo "  format_check        -- Check source code formatting without making changes"
	@echo "  cabalfmt            -- Apply cabal formatting with cabal-fmt"
	@echo "  cabalfmt_check      -- Check cabal files for formatting errors without making changes"
	@echo "  nixfmt              -- Apply nix formatting with nixfmt"
	@echo "  nixfmt_check        -- Check nix files for format errors"
	@echo "  lint                -- Check the sources with hlint"
	@echo "  refactor            -- Automatically apply hlint refactors, with prompt
	@echo "  readme_contents     -- Add table of contents to README"

hoogle: requires_nix_shell
	hoogle server --local

STACK_EXE_PATH = $(shell stack $(STACK_FLAGS) path --local-install-root)/bin

ifdef FLAGS
GHC_FLAGS = --ghc-options "$(FLAGS)"
endif

build: requires_nix_shell ardana-dollar.cabal
	cabal v2-build $(GHC_FLAGS)

test: requires_nix_shell ardana-dollar.cabal
	cabal v2-test --test-options="--contractMaxSuccess 25"

ghci: requires_nix_shell ardana-dollar.cabal
	cabal v2-repl $(GHC_FLAGS)

coverage: ardana-dollar.cabal
	nix-build --arg doCoverage true -A projectCoverageReport

# Source dirs to run fourmolu on
FORMAT_SOURCES := $(shell find -name '*.hs' -not -path './dist-*/*')

# Extensions we need to tell fourmolu about
FORMAT_EXTENSIONS := -o -XTypeApplications -o -XTemplateHaskell -o -XImportQualifiedPost -o -XPatternSynonyms -o -fplugin=RecordDotPreprocessor
# Run fourmolu formatter
format: requires_nix_shell
	fourmolu --mode inplace --check-idempotence $(FORMAT_EXTENSIONS) $(FORMAT_SOURCES)

# Check formatting (without making changes)
format_check: requires_nix_shell
	fourmolu --mode check --check-idempotence $(FORMAT_EXTENSIONS) $(FORMAT_SOURCES)

CABAL_SOURCES := $(shell git ls-tree -r HEAD --full-tree --name-only | grep -E '.*\.cabal' )

cabalfmt: requires_nix_shell
	cabal-fmt --inplace $(CABAL_SOURCES)

cabalfmt_check: requires_nix_shell
	cabal-fmt --check $(CABAL_SOURCES)

# Nix files to format
NIX_SOURCES := $(shell git ls-tree -r HEAD --full-tree --name-only | grep -E '.*\.nix' )

nixfmt: requires_nix_shell
	nixfmt $(NIX_SOURCES)

nixfmt_check: requires_nix_shell
	nixfmt --check $(NIX_SOURCES)

# Check with hlint, currently I couldn't get --refactor to work
lint: requires_nix_shell
	hlint $(FORMAT_SOURCES)

# Apply automatic hlint refactors, with prompt
refactor: requires_nix_shell
	for src in $(FORMAT_SOURCES) ; do hlint --refactor --refactor-options='-i -s' $$src ; done

readme_contents:
	echo "this command is not nix-ified, you may receive an error from npx"
	npx markdown-toc ./README.md --no-firsth1

# Target to use as dependency to fail if not inside nix-shell
requires_nix_shell:
	@ [ -v IN_NIX_SHELL ] || echo "The $(MAKECMDGOALS) target must be run from inside nix-shell"
	@ [ -v IN_NIX_SHELL ] || (echo "    run 'nix-shell --pure' first" && false)

costing: costing/*
	cabal run ardana-costing | grep "^Writing script:" | cut -d/ -f6 | tee costing-log.txt
