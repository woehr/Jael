{ nixpkgs ? import ../nix {}, compiler ? "ghc802" }:
let
  env = nixpkgs.haskell.packages.${compiler}.jael.env;
in
  nixpkgs.lib.overrideDerivation env (old: {
    buildInputs = old.buildInputs ++
      (with nixpkgs.haskell.packages.${compiler}; [
        #cabal-install
        #ghc-mod
        #stack
        hlint
        hdevtools
        #apply-refact
        #stylish-haskell
        #hasktags
      ]);
  })
