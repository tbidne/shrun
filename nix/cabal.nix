{ ghc-vers ? "ghc924"
, dev ? false
}:

let
  pkgs = import ((import ../default.nix).inputs.nixpkgs) { };
  compiler = pkgs.haskell.packages."${ghc-vers}";
  dev-tools = [
    compiler.haskell-language-server
  ];
in
pkgs.mkShell {
  buildInputs =
    [
      pkgs.cabal-install
      compiler.ghc
    ] ++ (if dev then dev-tools else [ ]);
}
