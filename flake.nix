{
  description = "Shrun is a tool for concurrently running shell commands.";
  inputs = {
    # nix
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    flake-parts.url = "github:hercules-ci/flake-parts";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    #haskell
    algebra-simple = {
      url = "github:tbidne/algebra-simple";
      inputs.flake-compat.follows = "flake-compat";
      inputs.flake-parts.follows = "flake-parts";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    bounds = {
      url = "github:tbidne/bounds";
      inputs.flake-compat.follows = "flake-compat";
      inputs.flake-parts.follows = "flake-parts";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    byte-types = {
      url = "github:tbidne/byte-types";
      inputs.flake-compat.follows = "flake-compat";
      inputs.flake-parts.follows = "flake-parts";
      inputs.nixpkgs.follows = "nixpkgs";

      inputs.algebra-simple.follows = "algebra-simple";
      inputs.bounds.follows = "bounds";
    };
    monad-effects = {
      url = "github:tbidne/monad-effects";
      inputs.flake-compat.follows = "flake-compat";
      inputs.flake-parts.follows = "flake-parts";
      inputs.nixpkgs.follows = "nixpkgs";

      inputs.algebra-simple.follows = "algebra-simple";
      inputs.bounds.follows = "bounds";
      inputs.smart-math.follows = "smart-math";
    };
    relative-time = {
      url = "github:tbidne/relative-time";
      inputs.flake-compat.follows = "flake-compat";
      inputs.flake-parts.follows = "flake-parts";
      inputs.nixpkgs.follows = "nixpkgs";

      inputs.algebra-simple.follows = "algebra-simple";
      inputs.bounds.follows = "bounds";
    };
    smart-math = {
      url = "github:tbidne/smart-math";
      inputs.flake-compat.follows = "flake-compat";
      inputs.flake-parts.follows = "flake-parts";
      inputs.nixpkgs.follows = "nixpkgs";

      inputs.algebra-simple.follows = "algebra-simple";
      inputs.bounds.follows = "bounds";
    };
  };
  outputs =
    { algebra-simple
    , bounds
    , byte-types
    , flake-compat
    , flake-parts
    , monad-effects
    , nixpkgs
    , relative-time
    , self
    , smart-math
    }:
    flake-parts.lib.mkFlake { inherit self; } {
      perSystem = { pkgs, ... }:
        let
          buildTools = c: with c; [
            cabal-install
            cabal-plan
            pkgs.gnumake
            pkgs.zlib
          ];
          devTools = c: with c; [
            ghcid
            haskell-language-server
          ];
          ghc-version = "ghc925";
          compiler = pkgs.haskell.packages."${ghc-version}".override {
            # 2022-12-17
            all-cabal-hashes = builtins.fetchurl {
              url = "https://github.com/commercialhaskell/all-cabal-hashes/archive/3a7b64a88d317ddd369445717fde18cfffce62cb.tar.gz";
              sha256 = "0cj8whjcq6z7yz1gj76yqr5ifp88zb217m26jz4nw4y9fyxbc5ip";
            };

            overrides = final: prev: {
              # https://github.com/ddssff/listlike/issues/23
              ListLike = hlib.dontCheck prev.ListLike;
            };
          };
          hlib = pkgs.haskell.lib;
          mkPkg = returnShellEnv: withDevTools:
            compiler.developPackage {
              inherit returnShellEnv;
              name = "shrun";
              root = ./.;
              modifier = drv:
                pkgs.haskell.lib.addBuildTools drv
                  (buildTools compiler ++
                    (if withDevTools then devTools compiler else [ ]));
              overrides = final: prev: with compiler; {
                algebra-simple = final.callCabal2nix "algebra-simple" algebra-simple { };
                bounds = final.callCabal2nix "bounds" bounds { };
                byte-types = final.callCabal2nix "byte-types" byte-types { };
                hedgehog = prev.hedgehog_1_2;
                monad-async =
                  final.callCabal2nix "monad-async"
                    "${monad-effects}/monad-async"
                    { };
                monad-exceptions =
                  final.callCabal2nix "monad-exceptions"
                    "${monad-effects}/monad-exceptions"
                    { };
                monad-env =
                  final.callCabal2nix "monad-env"
                    "${monad-effects}/monad-env"
                    { };
                monad-fs =
                  final.callCabal2nix "monad-fs"
                    "${monad-effects}/monad-fs"
                    { };
                monad-ioref =
                  final.callCabal2nix "monad-ioref"
                    "${monad-effects}/monad-ioref"
                    { };
                monad-optparse =
                  final.callCabal2nix "monad-optparse"
                    "${monad-effects}/monad-optparse"
                    { };
                monad-stm =
                  final.callCabal2nix "monad-stm"
                    "${monad-effects}/monad-stm"
                    { };
                monad-system-time =
                  final.callCabal2nix "monad-system-time"
                    "${monad-effects}/monad-system-time"
                    { };
                monad-terminal =
                  final.callCabal2nix "monad-terminal"
                    "${monad-effects}/monad-terminal"
                    { };
                monad-thread =
                  final.callCabal2nix "monad-thread"
                    "${monad-effects}/monad-thread"
                    { };
                monad-typed-process =
                  final.callCabal2nix "monad-typed-process"
                    "${monad-effects}/monad-typed-process"
                    { };
                package-version = pkgs.haskell.lib.doJailbreak prev.package-version;
                relative-time = final.callCabal2nix "relative-time" relative-time { };
                smart-math = final.callCabal2nix "smart-math" smart-math { };
                tasty-hedgehog = prev.tasty-hedgehog_1_4_0_0;
                toml-reader = final.callHackage "toml-reader" "0.2.0.0" { };
              };
            };
        in
        {
          packages.default = mkPkg false false;
          devShells.default = mkPkg true true;
          devShells.ci = mkPkg true false;
        };
      systems = [
        "x86_64-linux"
      ];
    };
}
