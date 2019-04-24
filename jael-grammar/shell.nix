with import ../default.nix {};
hpkgs.shellFor {
  packages = p: [ jael-grammar ];
  withHoogle = true;
  shellHook = ''
    #export HIE_HOOGLE_DATABASE="$NIX_GHC_DOCDIR"
    export HIE_HOOGLE_DATABASE="$NIX_GHC_LIBDIR/../../share/doc/hoogle/index.html"
  '';
}
