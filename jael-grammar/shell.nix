#import ../shell.nix "jael-grammar"

{ nixpkgs ? import ../pinned.nix }:
nixpkgs.haskellPackages.shellFor {
  packages = p: [ (p.callCabal2nix "jael-grammar" ./. {}) ];
  withHoogle = true;
  buildInputs = with nixpkgs.haskellPackages; [ cabal-install ghcide ];
}
