{
  description = "shell-run flake";
  inputs.nixpkgs.url = "nixpkgs/nixos-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
      let
        pkgs = import nixpkgs { inherit system; };
        mkPkg = returnShellEnv:
          pkgs.haskellPackages.developPackage {
            inherit returnShellEnv;
            name = "shell-run";
            root = ./.;
            modifier = drv:
              pkgs.haskell.lib.addBuildTools drv (with pkgs.haskellPackages; [
                cabal-fmt
                cabal-install
                haskell-language-server
                hlint
                implicit-hie
                ormolu
                pkgs.nixpkgs-fmt
              ]);
          };
      in
      {
        defaultPackage = mkPkg false;

        devShell = mkPkg true;
      });
}
