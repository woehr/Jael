self: super:
let

  inherit (super.pkgs) fetchFromGitHub;

  compiler = "ghc802";

  # Nov 21
  liquid-fixpoint-src = fetchFromGitHub {
    owner  = "ucsd-progsys";
    repo   = "liquid-fixpoint";
    rev    = "aba45f75b7d90a9c1980b835fe1b65ccb61dae4f";
    sha256 = "1a2ppn34iiz756d1jzcv6hgir74y84y9wg6i97v3fax7rnqi9fhb";
  };

  #liquid-fixpoint-src = fetchFromGitHub {
  #  owner  = "ucsd-progsys";
  #  repo   = "liquid-fixpoint";
  #  rev    = "e43aed1ccf3944597d5512738875dc037d4c25ff";
  #  sha256 = "1a2ppn34iiz756d1jzcv6hgir74y84y9wg6i97v3fax7rnqi9fhb";
  #};

in {
  haskell = super.haskell // {
    packages = super.haskell.packages // {
      "${compiler}" = super.haskell.packages.${compiler}.override {
        overrides = hself: hsuper: {

          ghc =  hsuper.ghc // { withPackages = hsuper.ghc.withHoogle; };
          ghcWithPackages = hself.ghc.withPackages;

          mkDerivation = args: hsuper.mkDerivation (args // {
#            doCheck = false;
#            enableLibraryProfiling = true;
#            enableExecutableProfiling = true;
          });

          llvm-hs = super.haskell.lib.dontCheck hsuper.llvm-hs;

          liquid-fixpoint = super.haskell.lib.dontCheck (
            hself.callCabal2nix "liquid-fixpoint" liquid-fixpoint-src {}
          );

          jael = hself.callCabal2nix "jael" ../jael {};
          jael-grammar = hself.callCabal2nix "jael-grammar" ../jael-grammar {};
        };
      };
    };
  };
}
