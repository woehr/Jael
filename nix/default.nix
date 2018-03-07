{}:
let
  pkgs = import
  # ghc822 in nixpkgs
  (fetchTarball https://github.com/NixOS/nixpkgs/archive/fc3ee7d18f3d7bf5ebe0b43741199732b85a04c1.tar.gz)
  #(fetchTarball https://github.com/NixOS/nixpkgs/archive/76ae77e37c6f516a71913ec0a454dac32b718cab.tar.gz)
  # Previous working ghc802
  #(fetchTarball https://github.com/NixOS/nixpkgs/archive/2509b629d77511ff5256f920663d541ebc43ea72.tar.gz)
  { overlays = [ (import ./overlay.nix) ]; };
in
pkgs
