{ compilerVersion
, stackYaml
, hash ? null
}:

let
  hash' =
    if hash == null
    then
      let
        lockJson = builtins.fromJSON (builtins.readFile ../flake.lock);
        nixpkgsKey = lockJson.nodes.root.inputs.nixpkgs;
      in
      lockJson.nodes.${nixpkgsKey}.locked.rev
    else hash;

  pkgs = import
    (fetchTarball {
      url = "https://github.com/NixOS/nixpkgs/archive/${hash'}.tar.gz";
    })
    { };
  compiler = pkgs.haskell.packages."${compilerVersion}";
in
pkgs.haskell.lib.buildStackProject {
  name = "shell-run";

  buildInputs = with pkgs; [
    git
    stack
    zlib.dev
    zlib.out
  ];

  ghc = compiler.ghc;

  STACK_YAML = stackYaml;
}
