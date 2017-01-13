{ mkDerivation, ansi-terminal, array, ascii-progress, async
, attoparsec, base, bifunctors, binary, boxes, bytestring, cereal
, cmdargs, containers, deepseq, directory, dotgen, fgl
, fgl-visualize, filemanip, filepath, ghc-prim, hashable, intern
, located-base, mtl, ocaml, parallel, parsec, pretty, process
, stdenv, syb, tasty, tasty-hunit, tasty-rerun, text, text-format
, time, transformers, unordered-containers, z3
, fetchgit
}:
mkDerivation {
  pname = "liquid-fixpoint";
  version = "0.5.0.1";
  src = fetchgit {
    url = https://github.com/ucsd-progsys/liquid-fixpoint;
    rev = "5fd9575b468f05d84b54b46850081d37f8e5f11b";
    sha256 = "0h2g4fmd43jj7mfg40qilnfp1p6l43sf7qm6dfpb4yy3lj2lngv6";
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    ansi-terminal array ascii-progress async attoparsec base bifunctors
    binary boxes bytestring cereal cmdargs containers deepseq directory
    dotgen fgl fgl-visualize filemanip filepath ghc-prim hashable
    intern located-base mtl parallel parsec pretty process syb text
    text-format time transformers unordered-containers
  ];
  executableHaskellDepends = [ base ];
  executableSystemDepends = [ ocaml ];
  testHaskellDepends = [
    base directory filepath process tasty tasty-hunit tasty-rerun text
  ];
  testSystemDepends = [ z3 ];
  homepage = "https://github.com/ucsd-progsys/liquid-fixpoint";
  description = "Predicate Abstraction-based Horn-Clause/Implication Constraint Solver";
  license = stdenv.lib.licenses.bsd3;
  doCheck = false;
}
