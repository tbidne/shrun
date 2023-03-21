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
    si-bytes = {
      url = "github:tbidne/si-bytes";
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
    inputs@{ algebra-simple
    , bounds
    , flake-compat
    , flake-parts
    , monad-effects
    , nixpkgs
    , relative-time
    , self
    , si-bytes
    , smart-math
    }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      perSystem = { pkgs, ... }:
        let
          buildTools = c: [
            c.cabal-install
            c.cabal-plan
            pkgs.gnumake
            pkgs.zlib
          ];
          devTools = c: [
            (hlib.dontCheck c.ghcid)
            (hlib.dontCheck c.haskell-language-server)
          ];
          ghc-version = "ghc944";
          compiler = pkgs.haskell.packages."${ghc-version}".override {
            overrides = final: prev: {
              # https://github.com/ddssff/listlike/issues/23
              ListLike = hlib.dontCheck prev.ListLike;
            };
          };
          hlib = pkgs.haskell.lib;
          mkPkg = returnShellEnv:
            compiler.developPackage {
              inherit returnShellEnv;
              name = "shrun";
              root = ./.;
              modifier = drv:
                pkgs.haskell.lib.addBuildTools drv
                  (buildTools compiler ++
                    (if returnShellEnv then devTools compiler else [ ]));
              overrides = final: prev: with compiler; {
                algebra-simple = final.callCabal2nix "algebra-simple" algebra-simple { };
                bounds = final.callCabal2nix "bounds" bounds { };
                hedgehog = prev.hedgehog_1_2;
                effects-async =
                  final.callCabal2nix "effects-async"
                    "${monad-effects}/effects-async"
                    { };
                effects-exceptions =
                  final.callCabal2nix "effects-exceptions"
                    "${monad-effects}/effects-exceptions"
                    { };
                effects-env =
                  final.callCabal2nix "effects-env"
                    "${monad-effects}/effects-env"
                    { };
                effects-fs =
                  final.callCabal2nix "effects-fs"
                    "${monad-effects}/effects-fs"
                    { };
                effects-ioref =
                  final.callCabal2nix "effects-ioref"
                    "${monad-effects}/effects-ioref"
                    { };
                effects-optparse =
                  final.callCabal2nix "effects-optparse"
                    "${monad-effects}/effects-optparse"
                    { };
                effects-stm =
                  final.callCabal2nix "effects-stm"
                    "${monad-effects}/effects-stm"
                    { };
                effects-time =
                  final.callCabal2nix "effects-time"
                    "${monad-effects}/effects-time"
                    { };
                effects-terminal =
                  final.callCabal2nix "effects-terminal"
                    "${monad-effects}/effects-terminal"
                    { };
                effects-thread =
                  final.callCabal2nix "effects-thread"
                    "${monad-effects}/effects-thread"
                    { };
                effects-typed-process =
                  final.callCabal2nix "effects-typed-process"
                    "${monad-effects}/effects-typed-process"
                    { };
                package-version = pkgs.haskell.lib.doJailbreak prev.package-version;
                relative-time = final.callCabal2nix "relative-time" relative-time { };
                si-bytes = final.callCabal2nix "si-bytes" si-bytes { };
                smart-math = final.callCabal2nix "smart-math" smart-math { };
                tasty-hedgehog = prev.tasty-hedgehog_1_4_0_0;
                toml-reader = final.callHackage "toml-reader" "0.2.0.0" { };
              };
            };
        in
        {
          packages.default = mkPkg false;
          devShells.default = mkPkg true;
        };
      systems = [
        "x86_64-darwin"
        "x86_64-linux"
      ];
    };
}
