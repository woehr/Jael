{}:
let
pkgs = import
  (fetchTarball https://github.com/NixOS/nixpkgs/archive/1dcb587d93e8a3f1de4c47939e4dd1e91c8546bc.tar.gz)
  { overlays = [ (import ./overlay.nix) ]; };
in
pkgs
