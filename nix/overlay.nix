self: super:
let

  inherit (super.pkgs) fetchFromGitHub;

  compiler = "ghc802";

  liquid-fixpoint-src = fetchFromGitHub {
    owner  = "ucsd-progsys";
    repo   = "liquid-fixpoint";
    rev    = "e43aed1ccf3944597d5512738875dc037d4c25ff";
    sha256 = "1a2ppn34iiz756d1jzcv6hgir74y84y9wg6i97v3fax7rnqi9fhb";
  };

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

          liquid-fixpoint = super.haskell.lib.dontCheck (
            hself.callCabal2nix "liquid-fixpoint" liquid-fixpoint-src {}
          );

          ghc-exactprint = super.haskell.lib.dontCheck hsuper.ghc-exactprint;
          llvm-hs = super.haskell.lib.dontCheck hsuper.llvm-hs;
          llvm-hs-pure = hself.llvm-hs-pure_4_1_0_0;

          jael = hself.callCabal2nix "jael" ../jael {};
          jael-grammar = hself.callCabal2nix "jael-grammar" ../jael-grammar {};
        };
      };
    };
  };
}
