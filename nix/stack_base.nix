{ compilerVersion
, stackYaml
, hash ? null
}:

let
  pkgs = (import ./lib.nix).get-pkgs hash;
  compiler = pkgs.haskell.packages."${compilerVersion}";
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

  STACK_YAML = stackYaml;
}
