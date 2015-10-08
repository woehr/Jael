{ nixpkgs ? (import <nixpkgs> {})
}:
let
  inherit (import ./gen-jael-drv.nix { inherit nixpkgs; }) jael-shell;
in
  jael-shell

