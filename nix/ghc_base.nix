{ compilerVersion ? "ghc8107"
}:

let
  pkgs = import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/b2737d4980a17cc2b7d600d7d0b32fd7333aca88.tar.gz") { };
  compiler = pkgs.haskell.packages."${compilerVersion}";
in
pkgs.mkShell {
  buildInputs =
    [
      pkgs.cabal-install
      compiler.ghc
    ];
}
