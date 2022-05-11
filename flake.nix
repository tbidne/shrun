{
  description = "Shell-Run is a tool for concurrently running shell commands.";
  inputs = {
    algebra-simple-src.url = "github:tbidne/algebra-simple";
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:nixos/nixpkgs?rev=98000933d72a97632caf0db0027ea3eb2e5e7f29";
    relative-time-src.url = "github:tbidne/relative-time";
  };
  outputs =
    { algebra-simple-src
    , flake-utils
    , nixpkgs
    , relative-time-src
    , self
    }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs { inherit system; };
      compilerVersion = "ghc922";
      compiler = pkgs.haskell.packages."${compilerVersion}";
      mkPkg = returnShellEnv:
        compiler.developPackage {
          inherit returnShellEnv;
          name = "shell-run";
          root = ./.;
          modifier = drv:
            pkgs.haskell.lib.addBuildTools drv (with compiler; [
              cabal-install
              haskell-language-server
              hlint
              ghcid
              ormolu
            ]);
          overrides = final: prev: with compiler; {
            algebra-simple =
              final.callCabal2nix "algebra-simple" algebra-simple-src { };
            package-version = pkgs.haskell.lib.doJailbreak prev.package-version;
            relative-time =
              final.callCabal2nix "relative-time" relative-time-src { };
            tasty-hedgehog = prev.tasty-hedgehog_1_2_0_0;
          };
        };
    in
    {
      defaultPackage = mkPkg false;

      devShell = mkPkg true;
    });
}
