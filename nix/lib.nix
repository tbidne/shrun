let
  lockJson = builtins.fromJSON (builtins.readFile ../flake.lock);
  nixpkgs-key = lockJson.nodes.root.inputs.nixpkgs;
  flake-hash = lockJson.nodes.${nixpkgs-key}.locked.rev;
  flake-pkgs = import
    (fetchTarball {
      url = "https://github.com/NixOS/nixpkgs/archive/${flake-hash}.tar.gz";
    })
    { };
  pkgs = hash: import
    (fetchTarball {
      url = "https://github.com/NixOS/nixpkgs/archive/${hash}.tar.gz";
    })
    { };

  # If a hash is provided, get the corresponding nixpkgs. Otherwise use
  # the flake's.
  get-pkgs = hash:
    if hash == null
    then flake-pkgs
    else pkgs hash;
in
{
  inherit get-pkgs;

}
