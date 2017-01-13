{ nixpkgs ? import <nixpkgs> {} }:
let
  compiler = "ghc7103";
in
  nixpkgs.overridePackages (pself: psuper: {
    haskellPackages = pself.haskell.packages.${compiler}.override {
      overrides = hself: hsuper: {
        llvm-general-pure = hself.callPackage ./llvm-general-pure.nix {};
        llvm-general = hself.callPackage ./llvm-general.nix { llvm-config = pself.llvm_39; };
        liquid-fixpoint = hself.callPackage ./liquid-fixpoint.nix {};
        prover = hself.callPackage ./prover.nix {};
        liquiddesugar = hself.callPackage ./liquiddesugar.nix {};
        liquidhaskell = hself.callPackage ./liquidhaskell.nix {};
        jael-grammar = hself.callPackage ./jael-grammar.nix {};
        jael = hself.callPackage ./jael.nix {};
      };
    };
  })
