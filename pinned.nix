with import ./config.nix;
let
  overlay = self: super:
    let lib = super.lib;
    hlib = super.haskell.lib;

    # ghc 8.4
    ghc-mod-src = self.fetchgit {
      url = "https://github.com/alanz/ghc-mod";
      branchName = "ghc-8.4-hie";
      rev = "3ccd528d4f08363ea363871fed4bb8a9a213cd2d";
      sha256 = "0nl0884pg15hvbrngb6dhazmx2j3v68hgq8lh345rihj01q35mzc";
    };

    in {
    haskell = super.haskell // {
      packages = super.haskell.packages // {
        ${GHC_VER_NIX} = super.haskell.packages.${GHC_VER_NIX}.override {
          overrides = hself: hsuper:
          {
            open-adt = hself.callCabal2nix "open-adt" ../open-adt/open-adt {};

            ghc = hsuper.ghc // { withPackages = hsuper.ghc.withHoogle; };
            ghcWithPackages = hself.ghc.withPackages;

            ghc-mod-core = hlib.doJailbreak (hself.callCabal2nix "ghc-mod-core" "${ghc-mod-src}/core" {});
            ghc-mod      = hlib.doJailbreak (hlib.dontCheck (hself.callCabal2nix "ghc-mod" ghc-mod-src {}));

            cabal-helper = hlib.doJailbreak hsuper.cabal-helper;
          };
        };
      };
    };
  }; # overlay

  pkgs = import (fetchTarball
      "https://github.com/NixOS/nixpkgs/archive/${NIXPKGS_VER}.tar.gz"
      ) { overlays = [ overlay ]; };
  hpkgs = pkgs.haskell.packages.${GHC_VER_NIX};

in pkgs // {
  inherit hpkgs;
  withHsTools = drv: pkgs.haskell.lib.addBuildDepends
    drv (with hpkgs; [brittany ghc-mod ghcid hlint]);
}
