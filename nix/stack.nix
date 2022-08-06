let
  pkgs = import ((import ../default.nix).inputs.nixpkgs) { };
  compiler = pkgs.haskell.packages."ghc923";
in
pkgs.haskell.lib.buildStackProject {
  name = "shrun";

  buildInputs = with pkgs; [
    git
    stack
    zlib.dev
    zlib.out
  ];

  ghc = compiler.ghc;

  STACK_YAML = "stack.yaml";
}
