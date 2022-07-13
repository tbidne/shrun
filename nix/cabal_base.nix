{ compilerVersion
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
pkgs.mkShell {
  buildInputs =
    [
      pkgs.cabal-install
      compiler.ghc
    ];
}
