{}:
  let p = import ../pinned-nixpkgs.nix;
  in p.stdenv.mkDerivation {
    name = "nix-shell-deps";
    buildInputs = [(p.haskell.packages.ghc822.ghcWithPackages
      (h: with h; [containers ghcid brittany liquidhaskell]))];
  }
