{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-22.11";
    devenv.url = "github:cachix/devenv";
  };

  outputs = inputs@{ flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [ inputs.devenv.flakeModule ];
      systems = [ "x86_64-linux" ];

      perSystem = { config, self', inputs', pkgs, system, ... }:
        let haskellPkgs = pkgs.haskell.packages.ghc943;
        in {
          packages.default = haskellPkgs.callPackage ./default.nix { };

          devShells.default = pkgs.mkShell {
            inputsFrom =
              [ self'.packages.default.env self'.devShells.devenv-shell ];
          };

          devenv.shells.devenv-shell = {
            packages = with haskellPkgs; [
              alex
              cabal-install
              happy
              haskell-language-server
            ];

            pre-commit.hooks = { cabal2nix.enable = true; };
            enterShell = ''
              echo hello devenv shell
            '';
          };
        };
    };
}
