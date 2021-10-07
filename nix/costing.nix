{ sourcesFile ? ./sources.json, system ? builtins.currentSystem
, sources ? import ./sources.nix { inherit system sourcesFile; }
, plutus ? import sources.plutus { }, deferPluginErrors ? true
, doCoverage ? false, contractMaxSuccess ? 50
, pkgs ? import <nixpkgs> {}}:
let
  project = import ./haskell.nix {
    inherit sourcesFile system sources plutus deferPluginErrors doCoverage;
  };
  rev = "${pkgs.util-linux}/bin/rev";
in rec {
  # These will be built by the CI.
  inherit (project.ardana-dollar.components.exes) ardana-costing;

  # This will run the tests within this build and produce the test logs as output
  check = plutus.pkgs.runCommand "run-costing" { } ''
    doCosting() {
      ${ardana-costing}/bin/ardana-costing | grep "^Writing script:" | ${rev} | cut -d/ -f1 | ${rev}
    }
    cmp ${project.ardana-dollar.src}/costing-log.txt <(doCosting)
    touch $out
  '';
}
