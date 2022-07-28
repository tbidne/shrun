{
  description = "Shrun is a tool for concurrently running shell commands.";
  inputs = {
    algebra-simple-src.url = "github:tbidne/algebra-simple";
    byte-types-src.url = "github:tbidne/byte-types";
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    relative-time-src.url = "github:tbidne/relative-time";
  };
  outputs =
    { algebra-simple-src
    , byte-types-src
    , flake-utils
    , nixpkgs
    , relative-time-src
    , self
    }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs { inherit system; };
      compilerVersion = "ghc923";
      compiler = pkgs.haskell.packages."${compilerVersion}";
      mkPkg = returnShellEnv:
        compiler.developPackage {
          inherit returnShellEnv;
          name = "shrun";
          root = ./.;
          modifier = drv:
            pkgs.haskell.lib.addBuildTools drv (with compiler; [
              cabal-install
              haskell-language-server
              ghcid
              ormolu
            ]);
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
      package.default = mkPkg false;

      devShell = mkPkg true;
    });
}
