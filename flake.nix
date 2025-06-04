{
  description = "Shrun is a tool for concurrently running shell commands.";
  inputs = {
    # nix
    flake-parts.url = "github:hercules-ci/flake-parts";
    nix-hs-utils.url = "github:tbidne/nix-hs-utils";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    # haskell
    algebra-simple = {
      url = "github:tbidne/algebra-simple";
      inputs.flake-parts.follows = "flake-parts";
      inputs.nix-hs-utils.follows = "nix-hs-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    bounds = {
      url = "github:tbidne/bounds";
      inputs.flake-parts.follows = "flake-parts";
      inputs.nix-hs-utils.follows = "nix-hs-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    exception-utils = {
      url = "github:tbidne/exception-utils";
      inputs.flake-parts.follows = "flake-parts";
      inputs.nix-hs-utils.follows = "nix-hs-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    fs-utils = {
      url = "github:tbidne/fs-utils";
      inputs.flake-parts.follows = "flake-parts";
      inputs.nix-hs-utils.follows = "nix-hs-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    monad-effects = {
      url = "github:tbidne/monad-effects";
      inputs.flake-parts.follows = "flake-parts";
      inputs.nix-hs-utils.follows = "nix-hs-utils";
      inputs.nixpkgs.follows = "nixpkgs";

      inputs.algebra-simple.follows = "algebra-simple";
      inputs.bounds.follows = "bounds";
      inputs.exception-utils.follows = "exception-utils";
      inputs.fs-utils.follows = "fs-utils";
      inputs.smart-math.follows = "smart-math";
    };
    relative-time = {
      url = "github:tbidne/relative-time";
      inputs.flake-parts.follows = "flake-parts";
      inputs.nix-hs-utils.follows = "nix-hs-utils";
      inputs.nixpkgs.follows = "nixpkgs";

      inputs.algebra-simple.follows = "algebra-simple";
      inputs.bounds.follows = "bounds";
    };
    si-bytes = {
      url = "github:tbidne/si-bytes";
      inputs.flake-parts.follows = "flake-parts";
      inputs.nix-hs-utils.follows = "nix-hs-utils";
      inputs.nixpkgs.follows = "nixpkgs";

      inputs.algebra-simple.follows = "algebra-simple";
      inputs.bounds.follows = "bounds";
    };
    smart-math = {
      url = "github:tbidne/smart-math";
      inputs.flake-parts.follows = "flake-parts";
      inputs.nix-hs-utils.follows = "nix-hs-utils";
      inputs.nixpkgs.follows = "nixpkgs";

      inputs.algebra-simple.follows = "algebra-simple";
      inputs.bounds.follows = "bounds";
    };
  };
  outputs =
    inputs@{
      flake-parts,
      nix-hs-utils,
      nixpkgs,
      self,
      ...
    }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      perSystem =
        { pkgs, system, ... }:
        let
          ghc-version = "ghc9101";
          compiler = pkgs.haskell.packages."${ghc-version}".override {
            overrides =
              final: prev:
              {
                dbus = prev.dbus_1_4_0;
                path = hlib.dontCheck prev.path_0_9_6;

                # optparse jailbreaks
                cabal-add = hlib.doJailbreak prev.cabal-add;
                fourmolu = hlib.doJailbreak prev.fourmolu;
                hspec-golden = hlib.doJailbreak prev.hspec-golden;
                ormolu = hlib.doJailbreak prev.ormolu;
                stan = hlib.doJailbreak prev.stan;
                tasty = hlib.doJailbreak prev.tasty;
                tasty-quickcheck = hlib.doJailbreak prev.tasty-quickcheck;
                tasty-rerun = hlib.doJailbreak prev.tasty-rerun;
                trial-optparse-applicative = hlib.doJailbreak prev.trial-optparse-applicative;

                # FIXME: Dev shell caching is terribly because I guess everything
                # uses optparse ugh. So we should wait until optparse 19 is the
                # default in nixpkgs i.e. stackage (nightly? or lts?).

                # Hopefully this is in the next nixpkgs merge (not at the time
                # of this comment):
                #
                #  https://github.com/NixOS/nixpkgs/pull/413046
                optparse-applicative = (
                  final.callHackageDirect {
                    pkg = "optparse-applicative";
                    ver = "0.19.0.0";
                    sha256 = "sha256-dhqvRILfdbpYPMxC+WpAyO0KUfq2nLopGk1NdSN2SDM=";
                  } { }
                );

                gitrev-typed = (
                  final.callHackageDirect {
                    pkg = "gitrev-typed";
                    ver = "0.1";
                    sha256 = "sha256-s7LEekR7NLe3CNhD/8uChnh50eGfaArrrtc5hoCtJ1A=";
                  } { }
                );
              }
              // nix-hs-utils.mkLibs inputs final [
                "algebra-simple"
                "bounds"
                "exception-utils"
                "fs-utils"
                "relative-time"
                "si-bytes"
                "smart-math"
              ]
              // nix-hs-utils.mkRelLibs "${inputs.monad-effects}/lib" final [
                "effects-async"
                "effects-env"
                "effects-fs"
                "effects-ioref"
                "effects-optparse"
                "effects-stm"
                "effects-terminal"
                "effects-thread"
                "effects-time"
                "effects-typed-process"
                "effects-unix-compat"
              ];
          };
          hlib = pkgs.haskell.lib;
          compilerPkgs = {
            inherit compiler pkgs;
          };
          mkPkg =
            returnShellEnv:
            nix-hs-utils.mkHaskellPkg {
              inherit compiler pkgs returnShellEnv;
              name = "shrun";
              root = ./.;

              devTools = [
                (hlib.dontCheck compiler.cabal-fmt)
                (hlib.dontCheck compiler.haskell-language-server)
                pkgs.nixfmt-rfc-style
              ];
              modifier =
                drv:
                drv.overrideAttrs (oldAttrs: {
                  SHRUN_HASH = "${self.rev or self.dirtyRev}";
                  SHRUN_MODIFIED = "${builtins.toString self.lastModified}";
                  SHRUN_SHORT_HASH = "${self.shortRev or self.dirtyShortRev}";
                });
            };
          stack-wrapped = pkgs.symlinkJoin {
            name = "stack";
            paths = [ pkgs.stack ];
            buildInputs = [ pkgs.makeWrapper ];
            postBuild = ''
              wrapProgram $out/bin/stack --add-flags "--no-nix --system-ghc"
            '';
          };

          pkgsMkDrv = {
            inherit pkgs;
            mkDrv = false;
          };
        in
        {
          packages.default = mkPkg false;
          devShells = {
            default = mkPkg true;

            notifyTests = pkgs.mkShell {
              buildInputs = nix-hs-utils.mkBuildTools compilerPkgs;
              shellHook = ''
                export NOTIFY_TESTS=1
              '';
            };

            stack = pkgs.mkShell {
              buildInputs = [
                compiler.ghc
                pkgs.zlib
                stack-wrapped
              ];
            };
          };

          apps = {
            format = nix-hs-utils.mergeApps {
              apps = [
                (nix-hs-utils.format (compilerPkgs // pkgsMkDrv))
                (nix-hs-utils.format-yaml pkgsMkDrv)
              ];
            };

            lint = nix-hs-utils.mergeApps {
              apps = [
                #(nix-hs-utils.lint (compilerPkgs // pkgsMkDrv))
                (nix-hs-utils.lint-yaml pkgsMkDrv)
              ];
            };

            #lint-refactor = nix-hs-utils.lint-refactor compilerPkgs;
          };
        };
      systems = [
        "aarch64-darwin"
        "aarch64-linux"
        "x86_64-darwin"
        "x86_64-linux"
      ];
    };
}
