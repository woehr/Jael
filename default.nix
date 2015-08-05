{ pkgs ? (import <nixpkgs> {}).pkgs
}:
let
  inherit (pkgs) stdenv;

  ghc = pkgs.haskell-ng.packages.ghc7102;

in {
  jael = ghc.callPackage (
    { mkDerivation
    , cabal-install, alex, BNFC, happy
    , array, base, classy-prelude, containers, HUnit, QuickCheck
    , template-haskell, test-framework, test-framework-hunit
    , test-framework-quickcheck2
    }:
    mkDerivation {
      pname = "Jael";
      version = "0.1.0.0";
      src = ./.;
      isLibrary = false;
      isExecutable = true;
      buildDepends = [ array base classy-prelude containers
        cabal-install
        alex
        BNFC
        happy
      ];
      testDepends = [
        array base classy-prelude containers HUnit QuickCheck template-haskell
        test-framework test-framework-hunit test-framework-quickcheck2
      ];
      description = "Jael: An Embedded Language";
      license = stdenv.lib.licenses.gpl2;
    }
  ) {};
}

