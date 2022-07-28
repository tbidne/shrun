{ compilerVersion
, hash ? null
}:

let
  pkgs = (import ./lib.nix).get-pkgs hash;
  compiler = pkgs.haskell.packages."${compilerVersion}";
in
pkgs.mkShell {
  buildInputs =
    [
      pkgs.cabal-install
      compiler.ghc
    ];
}
