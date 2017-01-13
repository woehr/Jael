{ mkDerivation, array, base, containers, directory, ghc, hpc
, stdenv, template-haskell, time
, fetchgit
}:
mkDerivation {
  pname = "liquiddesugar";
  version = "7.10.0.0";
  src = fetchgit {
    url = https://github.com/christetreault/liquiddesugar;
    rev = "792501fdbf75edbdeb496f9e16ee7ddf6fd2decf";
    sha256 = "040khlsmy2dky0lbsl2b643y96cj548j55ip2gih889rwljw0ji8";
  };
  libraryHaskellDepends = [
    array base containers directory ghc hpc template-haskell time
  ];
  homepage = "http://goto.ucsd.edu/liquidhaskell";
  description = "Haskell to GHC Core Desugarar for Liquid Haskell";
  license = stdenv.lib.licenses.bsd3;
}
