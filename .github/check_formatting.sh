#!/bin/bash

# Extensions necessary to tell fourmolu about
EXTENSIONS="-o -XTypeApplications -o -XTemplateHaskell -o -XImportQualifiedPost -o -XPatternSynonyms -o -fplugin=RecordDotPreprocessor"
SOURCES=$(find -name '*.hs' -not -path './dist-*/*')
fourmolu --mode check --check-idempotence $EXTENSIONS $SOURCES
