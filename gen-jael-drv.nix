{ nixpkgs ? (import <nixpkgs> {})
}:
let

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
      };
    };
  });

  jael-exprs = pkgs.stdenv.mkDerivation {
    name = "jael-nix-expr";
    src = ./jael.cabal;
    builder = builtins.toFile "builder.sh" ''
      source $stdenv/setup
      mkdir -p $out
      cp $src jael.cabal
      cabal2nix . > $out/default.nix
      cabal2nix . --shell > $out/shell.nix
    '';
    buildInputs = [pkgs.cabal2nix];
  };

  shell-inputs = [];

  # Override function common to default and shell derivations
  overrideJael = extra-inputs: drv: (pkgs.lib.overrideDerivation drv (old: {
    # src is incorrectly set to the location of jael-exprs
    src = ./.;
    # additional inputs for processing grammar and hacking in a shell
    buildInputs = old.buildInputs
               ++ extra-inputs
               ++ (with pkgs.haskellPackages; [alex BNFC happy]);
  }));

  # The derivations from the generated expressions
  jael-drv-default =
    pkgs.haskell.lib.overrideCabal
      (pkgs.haskellPackages.callPackage "${jael-exprs}/default.nix" {})
      (drv: {
        # Setup fails to find some _o_split files
        # when run with nix's haskell builder.
        # Need to investigate more
        enableSplitObjs = false;
        doHaddock = false;
      });
  jael-drv-shell = import "${jael-exprs}/shell.nix"
    { nixpkgs = pkgs;
    };

in {
  inherit pkgs;
  # Override generated derivations to fix src directory and add inputs
  jael = overrideJael [] jael-drv-default;
  jael-shell = overrideJael shell-inputs jael-drv-shell;
}

