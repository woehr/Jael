{ mkDerivation, aeson, array, base, bifunctors, bytestring, Cabal
, cereal, cmdargs, containers, data-default, deepseq, Diff
, directory, exceptions, filepath, fingertree, ghc, ghc-paths
, ghc-prim, hashable, hint, hpc, hscolour, liquid-fixpoint
, liquiddesugar, located-base, mtl, optparse-applicative, parsec
, pretty, process, QuickCheck, stdenv, stm, syb, tagged
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
    rev = "01155b7aec52a2a971467b920c7e1f2405f67a7c";
    sha256 = "0qbdp5fpa4fn1axqa5a6p053msykxdy6g99ln6wslc792mvgj1yi";
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson array base bifunctors bytestring Cabal cereal cmdargs
    containers data-default deepseq Diff directory exceptions filepath
    fingertree ghc ghc-paths ghc-prim hashable hpc hscolour
    liquid-fixpoint liquiddesugar located-base mtl parsec pretty
    process QuickCheck syb template-haskell temporary text
    text-format th-lift time transformers unordered-containers vector
  ];
  executableHaskellDepends = [
    base cmdargs deepseq ghc hint liquid-fixpoint located-base pretty
    process time
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
