{ mkDerivation, ansi-terminal, array, ascii-progress, async
, attoparsec, base, bifunctors, binary, boxes, bytestring, cereal
, cmdargs, containers, deepseq, directory, dotgen, fgl
, fgl-visualize, filemanip, filepath, ghc-prim, hashable, intern
, located-base, mtl, ocaml, parallel, parsec, pretty, process
, stdenv, syb
#, tasty, tasty-hunit, tasty-rerun
, text, text-format
, time, transformers, unordered-containers, z3
, fetchgit
}:
mkDerivation {
  pname = "liquid-fixpoint";
  version = "9.9.9.9";
  src = fetchgit {
    url = https://github.com/ucsd-progsys/liquid-fixpoint;
    rev = "cf3d1a35b90eeff48ddfd672bc11ff053dd7a746";
    sha256 = "1cvp6mrgq8yd0v2gm84dn5mcd4h3gv8w6w025k2nmb428mfqspwf";
  };
  isLibrary = true;
  isExecutable = true;
  doCheck = false;
  libraryHaskellDepends = [
    ansi-terminal array ascii-progress async attoparsec base bifunctors
    binary boxes bytestring cereal cmdargs containers deepseq directory
    dotgen fgl fgl-visualize filemanip filepath ghc-prim hashable
    intern located-base mtl parallel parsec pretty process syb text
    text-format time transformers unordered-containers
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
#    base directory filepath process tasty tasty-hunit tasty-rerun text
  ];
  testSystemDepends = [ z3 ];
  homepage = "https://github.com/ucsd-progsys/liquid-fixpoint";
  description = "Predicate Abstraction-based Horn-Clause/Implication Constraint Solver";
  license = stdenv.lib.licenses.bsd3;
}
