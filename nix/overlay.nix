self: super: {
  haskell = super.haskell // {
    packages = super.haskell.packages // {
      ghc7103 = super.haskell.packages.ghc7103.override {
        overrides = hself: hsuper: {
          liquid-fixpoint = hself.callPackage ./liquid-fixpoint.nix {};
          liquiddesugar = hself.callPackage ./liquiddesugar.nix {};
          liquidhaskell = hself.callPackage ./liquidhaskell.nix {};
        };
      };
      ghc801 = super.haskell.packages.ghc801.override {
        overrides = hself: hsuper: {
          llvm-general-pure = hself.callPackage ./llvm-general-pure.nix {};
          llvm-general = hself.callPackage ./llvm-general.nix { llvm-config = self.llvm_39; };

          liquid-fixpoint = hself.callPackage ./liquid-fixpoint.nix {};

          jael-grammar = hself.callPackage ./jael-grammar.nix {};
          jael = hself.callPackage ./jael.nix {};
        };
      };
    };
  };
}
