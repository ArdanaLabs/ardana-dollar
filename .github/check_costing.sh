#!/usr/bin/env bash

cmp costing-log.txt <(cabal run ardana-costing | grep "^Writing script:" | cut -d/ -f6)
