{ sourcesFile ? ./sources.json, system ? builtins.currentSystem
, sources ? import ./sources.nix { inherit system sourcesFile; }
, plutus ? import sources.plutus { }, deferPluginErrors ? true
, doCoverage ? false, contractMaxSuccess ? 50 }:
let
  project = import ./haskell.nix {
    inherit sourcesFile system sources plutus deferPluginErrors doCoverage;
  };
in rec {
  # These will be built by the CI.
  inherit (project.ardana-dollar.components) library;
  inherit (project.ardana-dollar.components.tests) ardana-dollar-test;
  inherit (project.ardana-dollar.components.exes) ardana-pab;

  # This will run the tests within this build and produce the test logs as output
  check = plutus.pkgs.runCommand "run-tests" { } ''
    ${ardana-dollar-test}/bin/ardana-dollar-test --contractMaxSuccess ${
      builtins.toString contractMaxSuccess
    } | tee $out
  '';
}
