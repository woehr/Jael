{ nixpkgs ? (import <nixpkgs> {})
}:
let
  pkgs = nixpkgs.pkgs;
  stdenv = pkgs.stdenv;
  ghc-str = "ghc7102";
  ghc = pkgs.haskell.packages."${ghc-str}";
  overrideCabal = pkgs.haskell.lib.overrideCabal;

  jael-exprs = stdenv.mkDerivation {
    name = "jael-default-expr";
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
  overrideJael = extra-inputs: drv: (stdenv.lib.overrideDerivation drv (old: {
    # src is incorrectly set to the location of jael-exprs
    src = ./.;
    # additional inputs for processing grammar and hacking in a shell
    buildInputs = old.buildInputs
               ++ extra-inputs
               ++ (with ghc; [alex BNFC happy]);
  }));

  # The derivations from the generated expressions
  jael-drv-default =
    overrideCabal (ghc.callPackage "${jael-exprs}/default.nix" {})
                  (drv: {
                    # Setup fails to find some _o_split files
                    # when run with nix's haskell builder.
                    # Need to investigate more
                    enableSplitObjs = false;
                    doHaddock = false;
                  });
  jael-drv-shell = (import "${jael-exprs}/shell.nix"
                     { inherit nixpkgs;
                       compiler = ghc-str;
                     }
                   );
in {
  # Override generated derivations to fix src directory and add inputs
  jael = overrideJael [] jael-drv-default;
  jael-shell = overrideJael shell-inputs jael-drv-shell;
}

