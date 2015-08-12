{ nixpkgs ? (import <nixpkgs> {})
}:
let
  pkgs = nixpkgs.pkgs;
  stdenv = pkgs.stdenv;
  ghc-str = "ghc7102";
  ghc = pkgs.haskell.packages."${ghc-str}";

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

  extra-inputs = (with ghc; [alex BNFC happy]);
  shell-inputs = (with ghc; [cabal-install]);

in {
  jael-drv-for = expr: let
    drv = (if expr == "default"
              then (ghc.callPackage "${jael-exprs}/default.nix" {})
              else (if expr == "shell"
                       then (import "${jael-exprs}/shell.nix" { inherit nixpkgs; compiler = ghc-str; })
                       else throw "Provide either default or shell as the argument.")
          );

    in stdenv.lib.overrideDerivation drv
      (old: {
        # src is incorrectly set to the location of jael-exprs
        src = ./.;
        # additional inputs for processing grammar and hacking in a shell
        buildInputs = old.buildInputs ++ extra-inputs ++
          (if expr == "shell"
            then shell-inputs
            else []
          );
      });
}
