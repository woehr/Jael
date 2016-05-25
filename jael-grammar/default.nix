{ mkDerivation, alex, array, base, BNFC, happy, mtl, stdenv }:
mkDerivation {
  pname = "jael-grammar";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ array base mtl ];
  buildDepends = [ alex BNFC happy ];
  description = "Grammar library for Jael";
  license = stdenv.lib.licenses.gpl2;
}
