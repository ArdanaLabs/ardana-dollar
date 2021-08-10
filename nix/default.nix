{ checkMaterialization ? false, sourcesFile ? ./sources.json
, system ? builtins.currentSystem }: rec {
  # Pratically, the only needed dependency is the plutus repository.
  sources = import ./sources.nix { inherit sourcesFile system; };

  # We're going to get everything from the main plutus repository. This ensures
  # we're using the same version of multiple dependencies such as nipxkgs,
  # haskell-nix, cabal-install, compiler-nix-name, etc.
  plutus = import sources.plutus { };
  pkgs = plutus.pkgs;

  haskell-nix = pkgs.haskell-nix;
  pab = import ./pab.nix { inherit plutus; };
}
