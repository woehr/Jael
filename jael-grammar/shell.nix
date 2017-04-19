{ nixpkgs ? import ../nix {}, compiler ? "ghc802" }:
let
  env = nixpkgs.haskell.packages.${compiler}.jael-grammar.env;
in
  nixpkgs.lib.overrideDerivation env (old: {
    buildInputs = old.buildInputs ++
      (with nixpkgs.haskell.packages.${compiler}; [
        ghc-mod apply-refact hlint stylish-haskell hasktags
      ]);
  })
