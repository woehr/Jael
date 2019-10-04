jael-package:
let
  inherit (import ./default.nix {}) pkgs hpkgs jael-ghc jael-pkgs;
  inherit (pkgs) lib makeWrapper stdenv writeShellScriptBin;

  hies = import (fetchTarball "https://github.com/infinisil/all-hies/tarball/master") {};
  jael-hie = hies.selection { selector = x: { "${jael-ghc}" = x."${jael-ghc}"; }; };

  ghcides = import (builtins.fetchTarball "https://github.com/hercules-ci/ghcide-nix/tarball/master") {};
  jael-ghcide = ghcides."ghcide-${jael-ghc}";

  theShell = hpkgs.shellFor {
    packages = _: [ jael-pkgs."${jael-package}" ];
    withHoogle = true;
    shellHook = ''
      export HIE_HOOGLE_DATABASE="$NIX_GHC_LIBDIR/../../share/doc/hoogle/index.html"
    '';
    buildInputs = with hpkgs; [
      cabal-install
      ghcid
      stack
      #jael-hie
      jael-ghcide
    ];
  };
in theShell
