p:
let ps = import ./default.nix {};
in ps.hpkgs.shellFor {
  packages = _: [ ps."${p}" ];
  withHoogle = true;
  shellHook = ''
    export HIE_HOOGLE_DATABASE="$NIX_GHC_LIBDIR/../../share/doc/hoogle/index.html"
  '';
}
