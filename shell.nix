{packages ? import ./. {}}:
let
  inherit (packages) pkgs plutus-starter;
  inherit (plutus-starter) haskell;
  inherit (packages) project;

in
  project.shellFor {
    withHoogle = false;

    nativeBuildInputs = with plutus-starter; [
      hlint
      cabal-install
      haskell-language-server
      stylish-haskell
      pkgs.niv
      cardano-repo-tool
    ];
  }

/*
{ lib
, sourcesFile ? ./nix/sources.json, system ? builtins.currentSystem
, sources ? import ./nix/sources.nix { inherit system sourcesFile; }
, plutus-latest ? import sources.plutus-latest { }
, plutus ? import sources.plutus { }
, pab ? (import ./nix/default.nix { inherit sourcesFile system; }).pab }:

let
  project = (import nix/pkgs/haskell/haskell.nix {
    inherit sourcesFile sources plutus;
    deferPluginErrors = true;
  });
  inherit (plutus) pkgs;
in (project.shellFor (pab.env_variables // {

  # Select packages who's dependencies should be added to the shell env
  packages = ps: [ ];

  # Select packages which should be added to the shell env, with their dependencies
  # Should try and get the extra cardano dependencies in here...
  additional = ps:
    with ps; [
      plutus-pab
      plutus-tx
      plutus-tx-plugin
      plutus-contract
      plutus-ledger-api
      pab.plutus_ledger_with_docs
      plutus-core
      playground-common
      prettyprinter-configurable
      plutus-use-cases
    ];

  withHoogle = true;

  # Extra haskell tools (arg passed on to mkDerivation)
  # Using the plutus.pkgs to use nixpkgs version from plutus (nixpkgs-unstable, mostly)
  propagatedBuildInputs = with pkgs;
    [
      # Haskell Tools
      cabal-install
      plutus.plutus.hlint
      haskellPackages.fourmolu
      nixfmt
      haskellPackages.cabal-fmt

      # Using plutus-latest, we get access to hls with ghc 8.10.4.20210212
      plutus-latest.plutus.haskell-language-server

      # hls doesn't support preprocessors yet so this has to exist in PATH
      haskellPackages.record-dot-preprocessor

      # Make building with --pure shell possible
      git
      gcc
      cacert

      # For readme-toc
      nodePackages.npm

      # Graphviz Diagrams for documentation
      graphviz
      imagemagick

      ### Pab
      pab.plutus_pab_client

      ### Example contracts
      plutus.plutus-currency
      plutus.plutus-atomic-swap

    ] ++ (builtins.attrValues pab.plutus_pab_exes);

  nativeBuildInputs = (with plutus.pkgs;
    [ cacert z3 zlib libsodium git cacert pkg-config ]
    ++ (lib.optionals (!stdenv.isDarwin) [ rPackages.plotly R ]));

}))
*/
