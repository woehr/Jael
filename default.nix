{ pkgs ? (import <nixpkgs> {}).pkgs
}:
let
  inherit (pkgs) stdenv;

  ghc = pkgs.haskell-ng.packages.ghc784;

in {
  jael = ghc.callPackage (
    { mkDerivation, alex, base, BNFC, cabal-install, classy-prelude, happy }:
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
      ];
      description = "Jael: An Embedded Language";
      license = stdenv.lib.licenses.gpl2;
    }
  ) {};
}

