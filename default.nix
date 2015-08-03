{ pkgs ? (import <nixpkgs> {}).pkgs
}:
let
  inherit (pkgs) stdenv;

  ghc = pkgs.haskell-ng.packages.ghc7102;

in {
  jael = ghc.callPackage (
    { mkDerivation, cabal-install
    , alex, BNFC, happy
    , base, classy-prelude, containers, either, mtl
    }:
    mkDerivation {
      pname = "Jael";
      version = "0.1.0.0";
      src = ./.;
      isLibrary = false;
      isExecutable = true;
      buildDepends = [
        cabal-install

        alex
        BNFC
        happy

        base
        classy-prelude
        containers
        either
        mtl
      ];
      description = "Jael: An Embedded Language";
      license = stdenv.lib.licenses.gpl2;
    }
  ) {};
}

