{ pkgs ? import <nixpkgs> {} }:
let jael = pkgs.haskell.packages.ghc8107.callPackage (import ./default.nix) {};
in pkgs.lib.listToAttrs (builtins.map (d: { name = builtins.replaceStrings ["."] ["_"] d.name; value = d; }) jael.env.nativeBuildInputs)
