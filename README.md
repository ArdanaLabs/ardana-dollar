# Ardana Dollar Stablecoin

## Build and development environment setup instructions

The build requires installed `nix`. Also to avoid building
large dependencies we use IOHK's nix binary cache.
Here are the instructions to set up the binary cache:
https://github.com/input-output-hk/plutus/tree/4551bba244a2f3ff890c4d2fd12a105692126d8d#iohk-binary-cache

Nix-based build: run

```
nix-build -A ardana-dollar.components.exes.ardana-pab
```

in the repository root directory. You should obtain an executable
under `./result/bin/ardana-pab`

The project can also be built inside a nix shell with cabal:

```
nix-shell
cabal build
```

## Code coverage report

Code coverage report can be produced with

```
make coverage
```

or

```
nix-build --arg doCoverage true -A projectCoverageReport
```

## Core git branches

`main` branch will hold the current working demo

`staging` is the development branch

Development is done in separate branches originating from `staging` branch.
(feature-branch approach).
All pull requests from these branches should be filed against `staging` (not `main`).
`main` and `staging` will be merged at regular intervals
(at around 1 week)

## Directory structure

- `pab/` - executable demo code
- `src/ArdanaDollar/` - CDP vaults (curretly demo stage, also this directory structure will be reorganized)
- `src/ArdanaDollar/Buffer/` - stake pool
- `src/ArdanaDollar/DanaStakePool/` - stake pool
- `src/ArdanaDollar/Treasury/` - treasury
- `test/` - test executable code
