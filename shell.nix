#jael-package:
#let
#  inherit (import ./default.nix {}) pkgs hpkgs jael-ghc jael-pkgs;
#  inherit (pkgs) lib makeWrapper stdenv writeShellScriptBin;
#
#  ghcides = import (builtins.fetchTarball "https://github.com/hercules-ci/ghcide-nix/tarball/master") {};
#  jael-ghcide = ghcides."ghcide-${jael-ghc}";
#
#  theShell = hpkgs.shellFor {
#    packages = _: [ jael-pkgs."${jael-package}" ];
#    withHoogle = true;
#    buildInputs = with hpkgs; [
#      cabal-install
#      ghcid
#      jael-ghcide
#    ];
#  };
#in theShell
{ pkgs ? import ./pinned.nix }:
let
  inherit (pkgs.lib) attrVals;
  deps = builtins.fromJSON (builtins.readFile ./package-dependencies.dhall);
  ghc = pkgs.haskellPackages.ghcWithPackages (p: attrVals deps p);
in pkgs.mkShell {
  inputsFrom = [];
  buildInputs = [ ghc ] ++ (with pkgs.haskellPackages; [ cabal-install ghcide alex happy ]);
  shellHook = ''
    export NIX_GHC_LIBDIR=$(ghc --print-libdir)
  '';
}
