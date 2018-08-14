{ top, lib ? null, exes ? [], tests ? [] }:

let nixpkgs = import ./pinned-nixpkgs.nix; in with nixpkgs.lib;

assert top != null;
assert top ? name && top ? version;
assert !(lib == null && exes == [] && tests == []);
assert lib != null -> lib ? exposed-modules && lib ? build-depends;
assert all (x: x ? name) exes;
assert all (x: x ? name) tests;

let
  top-defaults = {
    build-type = "Simple";
    cabal-version = ">=1.10";
  };

  lib-defaults = {
    build-depends = "base";
    default-language = "Haskell2010";
  };

  exe-defaults = {
    main-is = "Main.hs";
    build-depends = "base";
    default-language = "Haskell2010";
  };

  test-defaults = {
    type = "exitcode-stdio-1.0";
    main-is = "Main.hs";
    build-depends = "base";
    default-language = "Haskell2010";
  };

  stanzaLines = s: mapAttrsToList (n: v: "${n}: ${v}\n") s;
  mkStanza  = n: s:
  ''
    ${n}
    ${concatMapStrings (l: "  ${l}") (stanzaLines s)}
  '';

  mkLibStanza = s: mkStanza "library" (lib-defaults // s);
  mkExeStanza = s: mkStanza "executable ${s.name}"
                            (exe-defaults // removeAttrs s ["name"]);
  mkTestStanza = s: mkStanza "test-suite ${s.name}"
                             (test-defaults // removeAttrs s ["name"]);

  top-stanza = concatStrings (stanzaLines (top-defaults // top));
  lib-stanza = if isNull lib then "" else mkLibStanza lib;
  exe-stanzas  = concatMapStringsSep "\n" mkExeStanza exes;
  test-stanzas = concatMapStringsSep "\n" mkTestStanza tests;

in nixpkgs.writeText "${top.name}.cabal" ''
  ${top-stanza}
  ${lib-stanza}
  ${exe-stanzas}
  ${test-stanzas}
''
