{ nixpkgs ? (import <nixpkgs> {})
}:
let
  inherit (import ./gen-jael-drv.nix { inherit nixpkgs; }) jael-drv-for;
in
  jael-drv-for "shell"
