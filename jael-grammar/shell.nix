{ nixpkgs ? import ../nix/pkgs.nix {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, alex, array, base, BNFC, happy, mtl, stdenv
      }:
      mkDerivation {
        pname = "jael-grammar";
        version = "0.1.0.0";
        src = ./.;
        libraryHaskellDepends = [ array base mtl ];
        buildDepends = [ alex BNFC happy ];
        description = "Grammar library for Jael";
        license = stdenv.lib.licenses.gpl2;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
