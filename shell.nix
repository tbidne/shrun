{ compiler ? "ghc8104"
, pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/ad47284f8b01f587e24a4f14e0f93126d8ebecda.tar.gz") {}
}:

let
  haskellDeps = ps: with ps; [
    hlint
    implicit-hie
  ];

  # NOTE: This has to be separate because if it's included above then we don't
  # get the exe on the PATH (no idea why).
  haskellOtherDeps = [ pkgs.haskellPackages.ormolu ];

  ghc = pkgs.haskell.packages.${compiler}.ghcWithPackages haskellDeps;

  otherDeps = [
    pkgs.cabal-install
  ];
in pkgs.mkShell {

  buildInputs =
    [ghc]
    ++ haskellOtherDeps
    ++ otherDeps;

  shellHook = ''
    alias dev-build='cabal v2-build --ghc-options="-Wwarn"'
    alias dev-unit='cabal v2-test unit --ghc-options="-Wwarn" --test-show-details=direct'
    alias dev-integration='cabal v2-test integration --ghc-options="-Wwarn" --test-show-details=direct'
    alias dev-functional='cabal v2-test functional --ghc-options="-Wwarn" --test-show-details=direct'
    alias dev-test='cabal v2-test --ghc-options="-Wwarn" --test-show-details=direct'
    alias dev-run='cabal v2-run shell-run --ghc-options="-Wwarn"'
  '';
}
