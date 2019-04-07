{ pkgs ? import <nixos1903> {} }:
let
  hpkgs = pkgs.haskell.packages.ghc864;
  f = hpkgs.callCabal2nix;
in rec {
  inherit hpkgs;
  open-adt          = f "open-adt"          ../open-adt/open-adt { };
  jael-grammar      = f "jael-grammar"      ./jael-grammar       { };
  jael-grammar-test = f "jael-grammar-test" ./jael-grammar-test  { inherit jael-grammar; };
  jael-types        = f "jael-types"        ./jael-types         { inherit open-adt; };
  jael-pp           = f "jael-pp"           ./jael-pp            { inherit jael-types; };
  jael              = f "jael"              ./jael               { inherit jael-grammar jael-types; };
}
