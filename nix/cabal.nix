{ ghc-vers ? "ghc942"
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
  buildInputs = with pkgs;
    [
      pkgs.cabal-install
      compiler.ghc
      zlib.dev
      zlib.out
    ] ++ (if dev then dev-tools else [ ]);
}
