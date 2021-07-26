{ pkgs
, sources
, plutus
, haskell-nix
, checkMaterialization ? false
, enableHaskellProfiling ? false
}:
let
  gitignore-nix = pkgs.callPackage plutus.plutus.lib.gitignore-nix { };

  compiler-nix-name = plutus.plutus.haskell.compiler-nix-name;

  agdaWithStdlib = plutus.plutus.agdaWithStdlib;

  haskell = pkgs.callPackage ./haskell {
    inherit enableHaskellProfiling;
    inherit checkMaterialization;
    inherit agdaWithStdlib;
    inherit gitignore-nix sources haskell-nix;
#    inherit compiler-nix-name; # Use the same GHC version as plutus
  };

  hlint = plutus.plutus.hlint;

  cabal-install = plutus.plutus.cabal-install;

  stylish-haskell = plutus.plutus.stylish-haskell;

  haskell-language-server = plutus.plutus.haskell-language-server;

  cardano-repo-tool = plutus.plutus.cardano-repo-tool;

in
{
  inherit haskell;
  inherit hlint;
  inherit cabal-install;
  inherit stylish-haskell;
  inherit haskell-language-server;
  inherit cardano-repo-tool;
}
