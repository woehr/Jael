{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  jael-grammar = haskellPackages.callPackage ../jael-grammar {};
  liquid-fixpoint = haskellPackages.callPackage ./liquid-fixpoint.nix {};

  f = { mkDerivation, array, base-noprelude, base-prelude
      , containers, HUnit, lens
      , llvm-general, llvm-general-pure, mtl-prelude, placeholders
      , QuickCheck, recursion-schemes, stdenv, template-haskell
      , test-framework, test-framework-hunit, test-framework-quickcheck2
      , text, wl-pprint-text
      }:
      mkDerivation {
        pname = "jael";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          array base-noprelude base-prelude containers jael-grammar lens
          liquid-fixpoint llvm-general llvm-general-pure mtl-prelude
          placeholders recursion-schemes text wl-pprint-text
        ];
        executableHaskellDepends = [ base-noprelude base-prelude ];
        testHaskellDepends = [
          base-noprelude base-prelude containers HUnit jael-grammar
          llvm-general-pure QuickCheck template-haskell test-framework
          test-framework-hunit test-framework-quickcheck2
        ];
        description = "Jael: An Embedded Language";
        license = stdenv.lib.licenses.gpl2;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
