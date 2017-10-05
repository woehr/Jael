{}:
let
  pkgs = import
  (fetchTarball https://github.com/NixOS/nixpkgs/archive/2509b629d77511ff5256f920663d541ebc43ea72.tar.gz)
#  (fetchTarball https://github.com/NixOS/nixpkgs/archive/26cf940da87e5f1059d90e7bcd63c22f819421b3.tar.gz)
#  (fetchTarball https://github.com/NixOS/nixpkgs/archive/1dcb587d93e8a3f1de4c47939e4dd1e91c8546bc.tar.gz)
#  (fetchTarball https://github.com/NixOS/nixpkgs/archive/dc4e2cd70fd071f08cd5ba8db5ccd3d4dc68b197.tar.gz)
  { overlays = [ (import ./overlay.nix) ]; };
in
pkgs
