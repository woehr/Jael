{ mkDerivation, base, containers, HUnit, mtl, parsec, QuickCheck
, stdenv, template-haskell, test-framework, test-framework-hunit
, test-framework-quickcheck2, transformers, transformers-compat
, fetchgit
}:
mkDerivation {
  pname = "llvm-general-pure";
  version = "3.9.0.0";
  src = (import ./llvm-general-src.nix { inherit fetchgit; }) + "/llvm-general-pure";
  libraryHaskellDepends = [
    base containers mtl parsec template-haskell transformers
    transformers-compat
  ];
  testHaskellDepends = [
    base containers HUnit mtl QuickCheck test-framework
    test-framework-hunit test-framework-quickcheck2 transformers
    transformers-compat
  ];
  homepage = "http://github.com/bscarlet/llvm-general/";
  description = "Pure Haskell LLVM functionality (no FFI)";
  license = stdenv.lib.licenses.bsd3;
}
