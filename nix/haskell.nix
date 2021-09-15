{ sourcesFile ? ./sources.json, system ? builtins.currentSystem
, sources ? import ./sources.nix { inherit system sourcesFile; }
, plutus ? import sources.plutus { }, deferPluginErrors ? true
, doCoverage ? false }:
let inherit (plutus) pkgs;
in pkgs.haskell-nix.cabalProject rec {
  src = pkgs.haskell-nix.haskellLib.cleanGit {
    name = "ardana-dollar";
    src = ./..;
  };

  # Plutus uses a patched GHC. And so shall we.
  compiler-nix-name = "ghc810420210212";

  # -- Materialization
  # See https://input-output-hk.github.io/haskell.nix/tutorials/materialization/:
  # Update using:
  #   nix-build default.nix 2>&1 | grep -om1 '/nix/store/.*-updateMaterialized' | bash
  # plan-sha256 = "0m56bhk9w3v1zqpig84f9krrp6sqg21w0vxbjiqcxz8n7c39aw54";
  # materialized = ./materialization/ardana-dollar.materialized;

  modules = [{
    packages = {
      eventful-sql-common.doHaddock = false;
      eventful-sql-common.ghcOptions = [''
        -XDerivingStrategies -XStandaloneDeriving -XUndecidableInstances
                -XDataKinds -XFlexibleInstances -XMultiParamTypeClasses''];
      marlowe.doHaddock = deferPluginErrors;
      marlowe.flags.defer-plugin-errors = deferPluginErrors;

      plutus-use-cases.doHaddock = deferPluginErrors;
      plutus-use-cases.flags.defer-plugin-errors = deferPluginErrors;

      plutus-ledger.doHaddock = deferPluginErrors;
      plutus-ledger.flags.defer-plugin-errors = deferPluginErrors;

      cardano-crypto-praos.components.library.pkgconfig =
        pkgs.lib.mkForce [ [ pkgs.libsodium-vrf ] ];
      cardano-crypto-class.components.library.pkgconfig =
        pkgs.lib.mkForce [ [ pkgs.libsodium-vrf ] ];

      # This allows us to generate .tix coverage files, which could be useful?
      "${src.name}".components.library.doCoverage = doCoverage;
    };
  }];

  # Using this allows us to leave these nix-specific hashes _out_ of cabal.project
  # Normally, they'd be placed under the `source-repository-package` section as a comment like so:
  # `--sha256: ...`
  sha256map = {
    "https://github.com/input-output-hk/plutus.git"."${sources.plutus.rev}" =
      sources.plutus.sha256;

    "https://github.com/Quid2/flat.git"."ee59880f47ab835dbd73bea0847dab7869fc20d8" =
      "1lrzknw765pz2j97nvv9ip3l1mcpf2zr4n56hwlz0rk7wq7ls4cm";
    "https://github.com/shmish111/purescript-bridge.git"."6a92d7853ea514be8b70bab5e72077bf5a510596" =
      "13j64vv116in3c204qsl1v0ajphac9fqvsjp7x3zzfr7n7g61drb";
    "https://github.com/shmish111/servant-purescript.git"."a76104490499aa72d40c2790d10e9383e0dbde63" =
      "11nxxmi5bw66va7psvrgrw7b7n85fvqgfp58yva99w3v9q3a50v9";
    "https://github.com/input-output-hk/cardano-base"."cb0f19c85e5bb5299839ad4ed66af6fa61322cc4" =
      "0dnkfqcvbifbk3m5pg8kyjqjy0zj1l4vd23p39n6ym4q0bnib1cq";
    "https://github.com/input-output-hk/cardano-crypto.git"."07397f0e50da97eaa0575d93bee7ac4b2b2576ec" =
      "06sdx5ndn2g722jhpicmg96vsrys89fl81k8290b3lr6b1b0w4m3";
    "https://github.com/input-output-hk/cardano-ledger-specs"."d5b184a820853c7ba202efd615b8fadca1acb52c" =
      "04k5p6qwmfdza65gl5319r1ahdfwjnyqgzpfxdx0x2g5jcbimar4";
    "https://github.com/input-output-hk/cardano-prelude"."fd773f7a58412131512b9f694ab95653ac430852" =
      "02jddik1yw0222wd6q0vv10f7y8rdgrlqaiy83ph002f9kjx7mh6";
    "https://github.com/input-output-hk/goblins"."cde90a2b27f79187ca8310b6549331e59595e7ba" =
      "17c88rbva3iw82yg9srlxjv2ia5wjb9cyqw44hik565f5v9svnyg";
    "https://github.com/input-output-hk/iohk-monitoring-framework"."808724ff8a19a33d0ed06f9ef59fbd900b08553c" =
      "0298dpl29gxzs9as9ha6y0w18hqwc00ipa3hzkxv7nlfrjjz8hmz";
    "https://github.com/input-output-hk/ouroboros-network"."877ce057ff6fb086474c8eaad53f2b7f0e0fce6b" =
      "1kp0qysfy3hl96a3a61rijascq36f1imh3z4jy0vyiygb6qrv47z";
    "https://github.com/input-output-hk/cardano-node.git"."ed11e8b6429d4af1cb2539460e5cb2283a06b2dc" =
      "1wvr3zzl37i1fn5y9ni027rqw5bhh25z1bacvcaapxxjgdn38lbq";
    "https://github.com/input-output-hk/optparse-applicative"."7497a29cb998721a9068d5725d49461f2bba0e7a" =
      "1gvsrg925vynwgqwplgjmp53vj953qyh3wbdf34pw21c8r47w35r";
    "https://github.com/input-output-hk/Win32-network"."3825d3abf75f83f406c1f7161883c438dac7277d" =
      "19wahfv726fa3mqajpqdqhnl9ica3xmf68i254q45iyjcpj1psqx";
    "https://github.com/input-output-hk/hedgehog-extras"."edf6945007177a638fbeb8802397f3a6f4e47c14" =
      "0wc7qzkc7j4ns2rz562h6qrx2f8xyq7yjcb7zidnj7f6j0pcd0i9";

    "https://github.com/Liqwid-Labs/plutus-extra.git"."831cacd1f29c2fdec93278eb86d532bd61d6e494" =
      "1bgbb5s19bdvc9bin9j4ymf43hxhilr6f4w6f8kq5249aw5p57xy";
  };
}
