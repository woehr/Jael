{ shell ? false }:
let
  lib-name = "libjaelg";

  pkgs = import ../pinned-nixpkgs.nix;
  inherit (pkgs) stdenv writeText;
  inherit (import ../config-vars.nix) GHC_VER_NIX GHC_PKGS_GRAMMAR;
  h = pkgs.haskell.packages."${GHC_VER_NIX}";
  hlib = pkgs.haskell.lib;
  inherit (pkgs.lib) concatStringsSep inNixShell;

  cabal-file = import ../cabal.nix {
    top = {
      name = lib-name;
      version = "0.0.0.0";
    };
    lib = {
      build-depends = concatStringsSep "," GHC_PKGS_GRAMMAR;
      exposed-modules = "Jael.Grammar";
      hs-source-dirs = "src";
      other-modules = concatStringsSep " " [
        "Jael.Grammar.Abs"
        "Jael.Grammar.ErrM"
        "Jael.Grammar.Lex"
        "Jael.Grammar.Par"
        "Jael.Grammar.Print"
      ];
    };
    exes = [];
    tests = [
      { name = "tests";
        build-depends = concatStringsSep "," GHC_PKGS_GRAMMAR;
        hs-source-dirs = "test";
      }
    ];
  };

  grammar-src = stdenv.mkDerivation {
    name = "${lib-name}-src";
    srcs = [ ./Grammar.cf ./src ./test cabal-file ];
    phases = [ "buildPhase" "installPhase" ];
    buildPhase =
    ''
      source $stdenv/setup

      for f in $srcs; do
        cp -r --no-preserve=mode $f $(basename $f | cut -d- -f2)
      done

      mkdir -p src/Jael/Grammar
      ${h.BNFC}/bin/bnfc --haskell --ghc -p Jael -d --alex3 -o src Grammar.cf
      ${h.alex}/bin/alex -o src/Jael/Grammar/Lex.hs src/Jael/Grammar/Lex.x
      ${h.happy}/bin/happy -gac -o src/Jael/Grammar/Par.hs src/Jael/Grammar/Par.y
      rm src/Jael/Grammar/Lex.x
      rm src/Jael/Grammar/Par.y
    '';
    installPhase = "cp -r . $out";
  };

  jaelg-pkg = h.callCabal2nix lib-name "${grammar-src}" {};

in if shell
  then
    (hlib.addBuildDepends jaelg-pkg (
      with h; [ brittany ghc-mod hlint ])
    ).env
    else
    jaelg-pkg
