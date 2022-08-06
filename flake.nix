{
  description = "Shrun is a tool for concurrently running shell commands.";
  inputs = {
    algebra-simple-src.url = "github:tbidne/algebra-simple";
    byte-types-src.url = "github:tbidne/byte-types";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    relative-time-src.url = "github:tbidne/relative-time";
  };
  outputs =
    { algebra-simple-src
    , byte-types-src
    , flake-compat
    , flake-utils
    , nixpkgs
    , relative-time-src
    , self
    }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs { inherit system; };
      buildTools = c: with c; [
        cabal-install
        pkgs.gnumake
        pkgs.zlib
      ];
      devTools = c: with c; [
        ghcid
        haskell-language-server
      ];
      ghc-version = "ghc923";
      compiler = pkgs.haskell.packages."${ghc-version}";
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
            algebra-simple =
              final.callCabal2nix "algebra-simple" algebra-simple-src { };
            byte-types =
              final.callCabal2nix "byte-types" byte-types-src { };
            package-version = pkgs.haskell.lib.doJailbreak prev.package-version;
            relative-time =
              final.callCabal2nix "relative-time" relative-time-src { };
            tasty-hedgehog = prev.tasty-hedgehog_1_2_0_0;
          };
        };
    in
    {
      packages.default = mkPkg false false;

      devShells.default = mkPkg true true;
      devShells.ci = mkPkg true false;
    });
}
