{
  description = "Shell-Run is a tool for concurrently running shell commands.";
  inputs = {
    algebra-simple-src.url = "github:tbidne/algebra-simple/main";
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    refined-extras-src.url = "github:tbidne/refined-extras/main";
  };
  outputs =
    { algebra-simple-src
    , flake-utils
    , nixpkgs
    , refined-extras-src
    , self
    }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs { inherit system; };
      compilerVersion = "ghc8107";
      compiler = pkgs.haskell.packages."${compilerVersion}";
      mkPkg = returnShellEnv:
        compiler.developPackage {
          inherit returnShellEnv;
          name = "shell-run";
          root = ./.;
          modifier = drv:
            pkgs.haskell.lib.addBuildTools drv (with compiler; [
              cabal-install
              cabal-plan
              haskell-language-server
              hlint
              ghcid
              ormolu
            ]);
          overrides = final: prev: with compiler; {
            algebra-simple =
              final.callCabal2nix "algebra-simple" algebra-simple-src { };
            optics-core = final.optics-core_0_4;
            optics-th = final.optics-th_0_4;
            refined-extras =
              final.callCabal2nix "refined-extras" refined-extras-src { };
          };
        };
    in
    {
      defaultPackage = mkPkg false;

      devShell = mkPkg true;
    });
}
