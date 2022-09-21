{ mkDerivation, array, base, bytestring, comonad, containers
, deepseq, deriving-compat, filepattern, free, hpack, hspec
, hspec-discover, lib, mtl, pretty-show, recursion-schemes, text
, transformers, tree-diff, uniform-pair, unordered-containers
}:
mkDerivation {
  pname = "jael";
  version = "0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    array base bytestring comonad containers deepseq deriving-compat
    filepattern free hspec hspec-discover mtl pretty-show
    recursion-schemes text transformers tree-diff uniform-pair
    unordered-containers
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    array base bytestring comonad containers deepseq deriving-compat
    filepattern free hspec hspec-discover mtl pretty-show
    recursion-schemes text transformers tree-diff uniform-pair
    unordered-containers
  ];
  testHaskellDepends = [
    array base bytestring comonad containers deepseq deriving-compat
    filepattern free hspec hspec-discover mtl pretty-show
    recursion-schemes text transformers tree-diff uniform-pair
    unordered-containers
  ];
  testToolDepends = [ hspec-discover ];
  prePatch = "hpack";
  homepage = "https://github.com/woehr/Jael#readme";
  license = lib.licenses.bsd3;
}
