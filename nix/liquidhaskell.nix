{ mkDerivation, aeson, array, base, bifunctors, bytestring, Cabal
, cereal, cmdargs, containers, data-default, deepseq, Diff
, directory, exceptions, filepath, fingertree, ghc, ghc-paths
, ghc-prim, hashable, hint, hpc, hscolour, liquid-fixpoint
, liquiddesugar, located-base, mtl, optparse-applicative, parsec
, pretty, process, prover, QuickCheck, stdenv, stm, syb, tagged
, tasty, tasty-ant-xml, tasty-hunit, tasty-rerun, template-haskell
, temporary, text, text-format, th-lift, time, transformers
, unordered-containers, vector, z3
, fetchgit
}:
mkDerivation {
  pname = "liquidhaskell";
  version = "0.6.0.0";
  src = fetchgit {
    url = "https://github.com/ucsd-progsys/liquidhaskell";
    rev = "52fd62b86c7d851d33eabf4c9dc8e81864b59353";
    sha256 = "1qh14gnrqb1ixh84smcgrh0mlczp97psaxgpyj3x40mrvbrni7sf";
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson array base bifunctors bytestring Cabal cereal cmdargs
    containers data-default deepseq Diff directory exceptions filepath
    fingertree ghc ghc-paths ghc-prim hashable hpc hscolour
    liquid-fixpoint liquiddesugar located-base mtl parsec pretty
    process prover QuickCheck syb template-haskell temporary text
    text-format th-lift time transformers unordered-containers vector
  ];
  executableHaskellDepends = [
    base cmdargs deepseq ghc hint liquid-fixpoint located-base pretty
    process prover time
  ];
  testHaskellDepends = [
    base containers directory filepath mtl optparse-applicative process
    stm tagged tasty tasty-ant-xml tasty-hunit tasty-rerun transformers
  ];
  testSystemDepends = [ z3 ];
  homepage = "http://goto.ucsd.edu/liquidhaskell";
  description = "Liquid Types for Haskell";
  license = stdenv.lib.licenses.bsd3;
  doCheck = false;
}
