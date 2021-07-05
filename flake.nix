{
  description = "Jael flake";

  inputs = {
    nixpkgs.url = "nixpkgs/release-20.09";
  };

  outputs = { self, nixpkgs }:
  let pkgs = import nixpkgs { system = "x86_64-linux"; };
      hpkgs = pkgs.haskell.packages.ghc8104;
      jael = hpkgs.callCabal2nix "jael" ./. {};
      ghc = hpkgs.ghcWithPackages ( p: jael.buildInputs );
  in {
    defaultPackage.x86_64-linux = jael;
    devShell.x86_64-linux = pkgs.mkShell {
      inputsFrom = [ jael ];
      shellHook = ''
        export NIX_GHC_LIBDIR=$(ghc --print-libdir)
      '';
    };
    apps.x86_64-linux = {
      gen-cabal = pkgs.writeScriptBin "jael-gen-cabal" ''
        ${hpkgs.hpack}/bin/hpack
      '';
    };
  };
}
