let
  pkgsTarball = https://nixos.org/channels/nixos-19.09/nixexprs.tar.xz;
in
{ pkgs ? import (fetchTarball pkgsTarball) {} }:
let
  jael-ghc = "ghc865";
  hpkgs = pkgs.haskell.packages."${jael-ghc}";
  f = hpkgs.callCabal2nix;
  jael-pkgs = rec {
    #open-adt          = f "open-adt"          ../open-adt/open-adt { };
    #jael-grammar      = f "jael-grammar"      ./jael-grammar       { inherit jael-types open-adt; };
    #jael-types        = f "jael-types"        ./jael-types         { inherit open-adt; };
    jael-grammar      = f "jael-grammar"      ./jael-grammar       { inherit jael-types; };
    jael-types        = f "jael-types"        ./jael-types         {};
    jael-pp           = f "jael-pp"           ./jael-pp            { inherit jael-types; };
    jael              = f "jael"              ./jael               { inherit jael-grammar jael-types; };
  };
in {
  inherit pkgs hpkgs jael-ghc jael-pkgs;
}
