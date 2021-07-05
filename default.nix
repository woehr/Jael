{ nixpkgs ? import <nixos2009> {} }:
  let inherit (nixpkgs) pkgs; #pkgs = import <nixpkgs> {}; # { system = "x86_64-linux"; };
      hpkgs = pkgs.haskell.packages.ghc8104;
      jael = hpkgs.callCabal2nix "jael" ./. {};
      ghc = hpkgs.ghcWithPackages ( p: jael.buildInputs );
      #ghc = hpkgs.ghcWithHoogle ( p: jael.buildInputs );
  in 
  [ ghc ] ++ (with hpkgs; [
    alex 
    cabal-install
    ghcid
    happy
  ])
