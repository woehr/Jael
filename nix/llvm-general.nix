{ mkDerivation, array, base, bytestring, Cabal, containers, HUnit
, llvm-config, llvm-general-pure, mtl, parsec, QuickCheck, stdenv
, template-haskell, test-framework, test-framework-hunit
, test-framework-quickcheck2, transformers, transformers-compat
, utf8-string
, fetchgit
}:
mkDerivation {
  pname = "llvm-general";
  version = "3.9.0.0";
  src = (import ./llvm-general-src.nix { inherit fetchgit; }) + "/llvm-general";
  setupHaskellDepends = [ base Cabal containers ];
  libraryHaskellDepends = [
    array base bytestring containers llvm-general-pure mtl parsec
    template-haskell transformers transformers-compat utf8-string
  ];
  libraryToolDepends = [ llvm-config ];
  testHaskellDepends = [
    base containers HUnit llvm-general-pure mtl QuickCheck
    test-framework test-framework-hunit test-framework-quickcheck2
    transformers transformers-compat
  ];
  homepage = "http://github.com/bscarlet/llvm-general/";
  description = "General purpose LLVM bindings";
  license = stdenv.lib.licenses.bsd3;
}
