{ nixpkgs ? import ../nix/pkgs.nix {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, array, base, base-prelude, containers, hspec
      , jael-grammar, lens, liquid-fixpoint, llvm-general
      , llvm-general-pure, mtl-prelude, placeholders, recursion-schemes
      , stdenv, text, wl-pprint-text
      }:
      mkDerivation {
        pname = "jael";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          array base base-prelude containers jael-grammar lens
          liquid-fixpoint llvm-general llvm-general-pure mtl-prelude
          placeholders recursion-schemes text wl-pprint-text
        ];
        executableHaskellDepends = [ base base-prelude ];
        testHaskellDepends = [
          base base-prelude containers hspec jael-grammar llvm-general-pure
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
