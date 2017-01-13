{ mkDerivation, base, hashable, liquid-fixpoint, parsec, stdenv
, unordered-containers
, fetchgit
}:
mkDerivation {
  pname = "prover";
  version = "0.1.0.0";
  src = fetchgit {
    url = https://github.com/ucsd-progsys/prover.git;
    rev = "a9be047a996b3a4aa76910494804e6b6565a0707";
    sha256 = "0b5gfkrl2n2lxmqmml6kvc0rhpyms4g4n0aa0n95wxv00md76lf1";
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base hashable liquid-fixpoint parsec unordered-containers
  ];
  executableHaskellDepends = [
    base hashable liquid-fixpoint parsec unordered-containers
  ];
  description = "Automatic Prover of Logical Predicates";
  license = stdenv.lib.licenses.bsd3;
}
