{ mkDerivation, ansi-terminal, array, ascii-progress, async
, attoparsec, base, bifunctors, binary, boxes, bytestring, cereal
, cmdargs, containers, deepseq, directory, filemanip, filepath
, ghc-prim, hashable, intern, located-base, mtl, parallel, parsec, pretty
, process, stdenv, syb, tasty, tasty-hunit, tasty-rerun, text
, text-format, transformers, unordered-containers, z3
, dotgen, fgl, fgl-visualize, fetchgit
}:
mkDerivation {
  pname = "liquid-fixpoint";
  version = "9.9.9.9";
  src = fetchgit {
    url    = "https://github.com/ucsd-progsys/liquid-fixpoint";
    rev    = "b9042d62cfdcce01e6a9f57449ee865f790d09ef";
    sha256 = "1rla96m7lrzsl1z2j9v94dgvhnc4z144yzddxqamabcmvvipcp8n";
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    ansi-terminal array ascii-progress async attoparsec base bifunctors
    binary boxes bytestring cereal cmdargs containers deepseq directory
    filemanip filepath ghc-prim hashable intern located-base mtl parallel parsec
    pretty process syb text text-format transformers
    unordered-containers
    dotgen fgl fgl-visualize
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base directory filepath process tasty tasty-hunit tasty-rerun text
  ];
  testSystemDepends = [ z3 ];
  homepage = "https://github.com/ucsd-progsys/liquid-fixpoint";
  description = "Predicate Abstraction-based Horn-Clause/Implication Constraint Solver";
  license = stdenv.lib.licenses.bsd3;
  doCheck = false;
}
