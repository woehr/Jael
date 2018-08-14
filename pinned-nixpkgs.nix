with import ./config-vars.nix;
let

  haskus-utils =
    { mkDerivation, base, containers, extra, file-embed, list-t, mtl
    , recursion-schemes, stdenv, stm, stm-containers, tasty
    , tasty-quickcheck, template-haskell, transformers, vector
    , pkgs
    }:
    mkDerivation {
      pname = "haskus-utils";
      version = "0.9.0.0";
      src = pkgs.fetchFromGitHub {
        owner = "haskus";
        repo = "haskus-utils";
        rev = "a85d0b3cfcdd64fa9058edc1b8a5025696e537cd";
        sha256 = "07ybyqc33nkir9vdf8kl8qk9ljwih3135l9as958h4h44qpikxbh";
      };
      libraryHaskellDepends = [
        base containers extra file-embed list-t mtl recursion-schemes stm
        stm-containers template-haskell transformers vector
      ];
      testHaskellDepends = [ base tasty tasty-quickcheck ];
      homepage = "http://www.haskus.org/system";
      description = "Haskus utility modules";
      license = stdenv.lib.licenses.bsd3;
    }
  ;

  cabal-helper = import ../cabal-helper;
  ghc-mod-core = import ../ghc-mod/core;
  ghc-mod = import ../ghc-mod;

  ghcid = import ../ghcid;

  overlay = self: super:
    let lib = super.lib;
    hlib = super.haskell.lib;
    # "Fix" the derivation
    fixDrv = hsDrv: hlib.dontHaddock (hlib.dontCheck (hlib.doJailbreak hsDrv));
    in {
    haskell = super.haskell // {
      packages = super.haskell.packages // {
        ${GHC_VER_NIX} = super.haskell.packages.${GHC_VER_NIX}.override {
          overrides = hself: hsuper:
          {
            ghc = hsuper.ghc // { withPackages = hsuper.ghc.withHoogle; };
            ghcWithPackages = hself.ghc.withPackages;

            # Doesn't build for one reason or another
            cabal-helper = fixDrv (hsuper.callPackage cabal-helper {});
            ghc-mod      = fixDrv (hsuper.callPackage ghc-mod      {});
            ghc-mod-core = fixDrv (hsuper.callPackage ghc-mod-core {});
            haskus-utils = fixDrv (hsuper.callPackage haskus-utils {});
            ListLike = hlib.addBuildDepends hsuper.ListLike [ hself.semigroups ];
            stm-containers = fixDrv hsuper.stm-containers;
            enclosed-exceptions = fixDrv hsuper.enclosed-exceptions;
            #llvm-hs-pretty = fixDrv hsuper.llvm-hs-pretty;
            liquidhaskell = fixDrv (hsuper.callPackage (import ../liquidhaskell) {});
            liquid-fixpoint = fixDrv (hsuper.callPackage (import ../liquidhaskell/liquid-fixpoint) {});

            # Updated dependencies
            llvm-hs = hsuper.llvm-hs.override { llvm-config = self.llvm_6; };

            # Never version
            #ghcid = fixDrv (hsuper.callPackage ghcid {});
            ghcid = fixDrv hsuper.ghcid;
          };
        };
      };
    };
  };
in
  import (fetchTarball
    "https://github.com/NixOS/nixpkgs/archive/${NIXPKGS_VER}.tar.gz"
  ) { overlays = [ overlay ]; }
