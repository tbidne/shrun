{ compiler ? "ghc8104"
, pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/ad47284f8b01f587e24a4f14e0f93126d8ebecda.tar.gz") {}
}:

let
  haskellDeps = ps: with ps; [
    implicit-hie
  ];

  ghc = pkgs.haskell.packages.${compiler}.ghcWithPackages haskellDeps;
in pkgs.mkShell {

  buildInputs = [
    ghc
  ];

  shellHook = ''
    alias dev-build='cabal v2-build --ghc-options="-Wwarning"'
    alias dev-test='cabal v2-test --ghc-options="-Wwarning"'
    alias dev-run='cabal v2-run shell-run --ghc-options="-Wwarning"'
  '';
}
