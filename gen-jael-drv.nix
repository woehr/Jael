{ nixpkgs ? (import <nixpkgs> {})
}:
let

  cabal-to-nix = cabal-dir:
    nixpkgs.stdenv.mkDerivation {
      name = "cabal-expr";
      src = cabal-dir;
      builder = builtins.toFile "builder.sh" ''
        source $stdenv/setup
        mkdir -p $out
        cabal2nix $src > $out/default.nix
        cabal2nix $src --shell > $out/shell.nix
      '';
      buildInputs = [nixpkgs.cabal2nix];
    };

  jael-exprs = cabal-to-nix ./jael;

  jael-grammar-src = nixpkgs.stdenv.mkDerivation {
    name = "jael-grammar-src";
    src = ./jael-grammar;
    builder = builtins.toFile "builder.sh" ''
      source $stdenv/setup
      mkdir -p $out/lib/Jael/Grammar
      cp -r $src/* $out
      bnfc --haskell-gadt --alex3 --ghc --functor -d -p Jael -o $out/lib $src/lib/Jael/Grammar.cf
      happy -gcai $out/lib/Jael/Grammar/Par.y
      alex  -g    $out/lib/Jael/Grammar/Lex.x
    '';
    buildInputs = with nixpkgs.haskellPackages; [alex BNFC happy];
  };

  jael-grammar-exprs = cabal-to-nix "${jael-grammar-src}" ;

  pkgs = nixpkgs.overridePackages (pSelf: pSuper: {
    haskellPackages = pSuper.haskellPackages.override {
      overrides = hSelf: hSuper:
      let liquid-fixpoint-git = pSuper.fetchgit {
        url    = "https://github.com/ucsd-progsys/liquid-fixpoint";
        rev    = "d90c7de1d07507e3fb2f93788cfbca8b7e233e4f";
        sha256 = "11kplsfdlf06ynghgjvn062pac562bmi7nc6fq6pqxhnvqxjljnw";
      };
      in
      { liquid-fixpoint = pSuper.lib.overrideDerivation
          (hSelf.callPackage
            (import "${liquid-fixpoint-git}/default.nix" {
              fetchgitLocal = abort "should not be used";
            })
            {}
          )
          (old: { src = liquid-fixpoint-git; doCheck = false; });

        jael = pSuper.haskell.lib.overrideCabal
          (hSelf.callPackage "${jael-exprs}/default.nix" {})
          (drv: {
            # Setup fails to find some _o_split files
            # when run with nix's haskell builder.
            # Need to investigate more
            enableSplitObjs = false;
            doHaddock = false;
          });

        jael-grammar = pSelf.lib.overrideDerivation
          (pSuper.haskell.lib.overrideCabal
            (hSelf.callPackage "${jael-grammar-exprs}/default.nix" {})
            (drv: { doHaddock = false; })
          )
          (old: { buildInputs = old.buildInputs ++ (with hSelf; [alex happy]); });
      };
    };
  });

in {
  inherit jael-exprs jael-grammar-src jael-grammar-exprs;
  inherit (pkgs.haskellPackages) jael jael-grammar;
  jael-shell = import "${jael-exprs}/shell.nix" { nixpkgs=pkgs; };
  jael-grammar-shell = import "${jael-grammar-exprs}/shell.nix" { nixpkgs=pkgs; };
}

