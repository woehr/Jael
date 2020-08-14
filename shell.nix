{ nixpkgs ? import ./pinned.nix }:
let
  inherit (nixpkgs) pkgs;
  inherit (pkgs.lib) attrVals;
  deps = builtins.fromJSON (builtins.readFile ./package-dependencies.dhall);
  ghc = pkgs.haskellPackages.ghcWithPackages (p: attrVals deps p);
in
pkgs.mkShell {
  inputsFrom = [ ];
  buildInputs = [ ghc ] ++ (with pkgs.haskellPackages;
    [
      brittany
      ghcid
      ghcide
      hlint
      cabal-install
      alex
      happy
    ]
  );
  shellHook = ''
    export NIX_GHC_LIBDIR=$(ghc --print-libdir)
  '';
}
